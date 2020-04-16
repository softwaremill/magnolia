/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package magnolia

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.language.existentials
import scala.reflect.macros._

/** the object which defines the Magnolia macro */
object Magnolia {
  import CompileTimeState._

  /** derives a generic typeclass instance for the type `T`
    *
    *  This is a macro definition method which should be bound to a method defined inside a Magnolia
    *  generic derivation object, that is, one which defines the methods `combine`, `dispatch` and
    *  the type constructor, `Typeclass[_]`. This will typically look like,
    *  <pre>
    *  object Derivation {
    *    // other definitions
    *    implicit def gen[T]: Typeclass[T] = Magnolia.gen[T]
    *  }
    *  </pre>
    *  which would support automatic derivation of typeclass instances by calling
    *  `Derivation.gen[T]` or with `implicitly[Typeclass[T]]`, if the implicit method is imported
    *  into the current scope.
    *
    *  If the `gen` is not `implicit`, semi-auto derivation is used instead, whereby implicits will
    *  not be generated outside of this ADT.
    *
    *  The definition expects a type constructor called `Typeclass`, taking one *-kinded type
    *  parameter to be defined on the same object as a means of determining how the typeclass should
    *  be genericized. While this may be obvious for typeclasses like `Show[T]` which take only a
    *  single type parameter, Magnolia can also derive typeclass instances for types such as
    *  `Decoder[Format, Type]` which would typically fix the `Format` parameter while varying the
    *  `Type` parameter.
    *
    *  While there is no "interface" for a derivation, in the object-oriented sense, the Magnolia
    *  macro expects to be able to call certain methods on the object within which it is bound to a
    *  method.
    *
    *  Specifically, for deriving case classes (product types), the macro will attempt to call the
    *  `combine` method with an instance of [[CaseClass]], like so,
    *  <pre>
    *    &lt;derivation&gt;.combine(&lt;caseClass&gt;): Typeclass[T]
    *  </pre>
    *  That is to say, the macro expects there to exist a method called `combine` on the derivation
    *  object, which may be called with the code above, and for it to return a type which conforms
    *  to the type `Typeclass[T]`. The implementation of `combine` will therefore typically look
    *  like this,
    *  <pre>
    *    def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  however, there is the flexibility to provide additional type parameters or additional
    *  implicit parameters to the definition, provided these do not affect its ability to be invoked
    *  as described above.
    *
    *  Likewise, for deriving sealed traits (coproduct or sum types), the macro will attempt to call
    *  the `dispatch` method with an instance of [[SealedTrait]], like so,
    *  <pre>
    *    &lt;derivation&gt;.dispatch(&lt;sealedTrait&gt;): Typeclass[T]
    *  </pre>
    *  so a definition such as,
    *  <pre>
    *    def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  will suffice, however the qualifications regarding additional type parameters and implicit
    *  parameters apply equally to `dispatch` as to `combine`.
    *  */
  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = Stack.withContext(c) { stack =>
    import c.universe._
    import c.internal._

    def error(message: String): Nothing = c.abort(c.enclosingPosition, s"magnolia: $message")

    val debug = c.macroApplication.symbol.annotations
      .find(_.tree.tpe <:< typeOf[debug])
      .flatMap(_.tree.children.tail.collectFirst {
        case Literal(Constant(s: String)) => s
        case tree if typeOf[debug].companion.decls.exists(_ == tree.symbol) => ""  // Default constructor, i.e. @debug or @debug()
        case other => error(s"Invalid argument $other in @debug annotation. Only string literals or empty constructor supported")
      })

    val magnoliaPkg = c.mirror.staticPackage("magnolia")
    val scalaPkg = c.mirror.staticPackage("scala")

    val repeatedParamClass = definitions.RepeatedParamClass
    val scalaSeqType = typeOf[Seq[_]].typeConstructor
    val javaAnnotationType = typeOf[java.lang.annotation.Annotation]

    val prefixType = c.prefix.tree.tpe
    val prefixObject = prefixType.typeSymbol
    val prefixName = prefixObject.name.decodedName

    object DeferredRef {
      private val symbol = symbolOf[Deferred.type].asClass.module

      def apply(searchType: Type, method: String): Tree =
        q"$symbol.apply[$searchType]($method)"

      def unapply(tree: Tree): Option[String] = tree match {
        case q"$module.apply[$_](${Literal(Constant(method: String))})"
          if module.symbol == symbol => Some(method)
        case _ => None
      }
    }

    val enclosingVals = Iterator
      .iterate(enclosingOwner)(_.owner)
      .takeWhile(encl => encl != null && encl != NoSymbol)
      .filter(_.isTerm)
      .map(_.asTerm)
      .filter(encl => encl.isVal || encl.isLazy)
      .toSet[Symbol]

    def knownSubclasses(sym: ClassSymbol): Set[Symbol] = {
      val children = sym.knownDirectSubclasses
      val (abstractTypes, concreteTypes) = children.partition(_.isAbstract)
      abstractTypes.map(_.asClass).flatMap(knownSubclasses(_)) ++ concreteTypes
    }

    def annotationsOf(symbol: Symbol): List[Tree] = {
      symbol.annotations.map(_.tree).filterNot(_.tpe <:< javaAnnotationType)
    }

    val typeDefs = prefixType.baseClasses.flatMap { cls =>
      cls.asType.toType.decls.filter(_.isType).find(_.name.toString == "Typeclass").map { tpe =>
        tpe.asType.toType.asSeenFrom(prefixType, cls)
      }
    }

    val typeConstructor = typeDefs.headOption.fold(
      error(s"the derivation $prefixObject does not define the Typeclass type constructor")
    )(_.typeConstructor)

    def checkMethod(termName: String, category: String, expected: String): Unit = {
      val firstParamBlock = extractParameterBlockFor(termName, category)
      if (firstParamBlock.lengthCompare(1) != 0)
        error(s"the method `$termName` should take a single parameter of type $expected")
    }

    def extractParameterBlockFor(termName: String, category: String): List[Symbol] = {
      val term = TermName(termName)
      val classWithTerm = c.prefix.tree.tpe.baseClasses
        .find(cls => cls.asType.toType.decl(term) != NoSymbol)
        .getOrElse(error(s"the method `$termName` must be defined on the derivation $prefixObject to derive typeclasses for $category"))

      classWithTerm.asType.toType.decl(term).asTerm.asMethod.paramLists.head
    }

    val firstParameterTypeName: Option[String] = {
      val parametersForCombine = extractParameterBlockFor("combine", "case classes")
      parametersForCombine.headOption.map(_.typeSignature.typeConstructor.typeSymbol.fullName)
    }

    val (isReadOnlyTypeclass, magCaseClassType, magParamType) = firstParameterTypeName match {
      case Some("magnolia.ReadOnlyCaseClass") => (true, tq"$magnoliaPkg.ReadOnlyCaseClass", tq"$magnoliaPkg.ReadOnlyParam")
      case Some("magnolia.CaseClass") => (false, tq"$magnoliaPkg.CaseClass", tq"$magnoliaPkg.Param")
      case _ => error("Parameter for `combine` needs be either magnolia.CaseClass or magnolia.ReadOnlyCaseClass")
    }

    // fullauto means we should directly infer everything, including external
    // members of the ADT, that isn't inferred by the compiler.
    //
    // semiauto means that we should directly derive only the sealed ADT but not
    // external members (i.e. things that are not a subtype of T).
    val fullauto = c.macroApplication.symbol.isImplicit
    val tSealed = weakTypeOf[T].typeSymbol.isClass && weakTypeOf[T].typeSymbol.asClass.isSealed
    def semiauto(s: Type): Boolean = tSealed && s <:< weakTypeOf[T]

    // Trees that contain Deferred references might not be self contained and should not be cached.
    def shouldCache(tree: Tree): Boolean = !tree.exists {
      case DeferredRef(_) => true
      case _ => false
    }

    val expandDeferred = new Transformer {
      override def transform(tree: Tree) = tree match {
        case DeferredRef(method) => q"${TermName(method)}"
        case _ => super.transform(tree)
      }
    }

    def deferredVal(name: TermName, tpe: Type, rhs: Tree): Tree = {
      val shouldBeLazy = rhs.exists {
        case DeferredRef(_) => true
        case tree => enclosingVals.contains(tree.symbol)
      }

      if (shouldBeLazy) q"lazy val $name: $tpe = $rhs"
      else q"val $name = $rhs"
    }

    def typeclassTree(genericType: Type, typeConstructor: Type, assignedName: TermName): Either[String, Tree] = {
      val searchType = appliedType(typeConstructor, genericType)
      val deferredRef = for (methodName <- stack find searchType)
        yield DeferredRef(searchType, methodName.decodedName.toString)

      deferredRef.fold {
        val path = ChainedImplicit(s"$prefixName.Typeclass", genericType.toString)
        val frame = stack.Frame(path, searchType, assignedName)
        stack.recurse(frame, searchType, shouldCache) {
          Option(c.inferImplicitValue(searchType))
            .filterNot(_.isEmpty)
            .orElse {
              if (!fullauto && !semiauto(genericType)) None
              else directInferImplicit(genericType, typeConstructor)
            }.toRight {
              val (top, paths) = stack.trace
              val missingType = top.fold(searchType)(_.searchType)
              val typeClassName = s"${missingType.typeSymbol.name.decodedName}.Typeclass"
              val genericType = missingType.typeArgs.head
              val trace = paths.mkString("    in ", "\n    in ", "\n")
              s"could not find $typeClassName for type $genericType\n$trace"
            }
        }
      } (Right(_))
    }

    def directInferImplicit(genericType: Type, typeConstructor: Type): Option[Tree] = {
      val genericTypeName = genericType.typeSymbol.name.decodedName.toString.toLowerCase
      val assignedName = TermName(c.freshName(s"${genericTypeName}Typeclass")).encodedName.toTermName
      val typeSymbol = genericType.typeSymbol
      val classType = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isRefinedType = PartialFunction.cond(genericType.dealias) { case _: RefinedType => true }
      val isCaseClass = classType.exists(_.isCaseClass)
      val isCaseObject = classType.exists(_.isModuleClass)

      val isSealedTrait = classType.exists(ct => ct.isSealed && !ct.isJavaEnum)
      val classAnnotationTrees = annotationsOf(typeSymbol)

      val primitives = Set(typeOf[Double],
                           typeOf[Float],
                           typeOf[Short],
                           typeOf[Byte],
                           typeOf[Int],
                           typeOf[Long],
                           typeOf[Char],
                           typeOf[Boolean],
                           typeOf[Unit])

      val isValueClass = genericType <:< typeOf[AnyVal] && !primitives.exists(_ =:= genericType)
      val resultType = appliedType(typeConstructor, genericType)
      val typeName = TermName(c.freshName("typeName"))

      def typeNameRec(t: Type): Tree = {
        val ts = t.typeSymbol
        val typeArgNames = t.typeArgs.map(typeNameRec(_))
        q"$magnoliaPkg.TypeName(${ts.owner.fullName}, ${ts.name.decodedName.toString}, $typeArgNames)"
      }

      val typeNameDef = q"val $typeName = ${typeNameRec(genericType)}"

      val result = if (isRefinedType) {
        error(s"could not infer $prefixName.Typeclass for refined type $genericType")
      } else if (isCaseObject) {
        val f = TypeName(c.freshName("F"))
        val classBody = if (isReadOnlyTypeclass) List(q"") else
        List(
          q"""
            override def construct[Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => Return): $genericType =
              ${genericType.typeSymbol.asClass.module}
           """,
          q"""
            def constructMonadic[$f[_], Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => $f[Return])(implicit monadic: _root_.mercator.Monadic[$f]): $f[$genericType] =
              monadic.point(${genericType.typeSymbol.asClass.module})
           """,
          q"""
            def constructEither[Err, PType](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => $scalaPkg.Either[Err, PType]): $scalaPkg.Either[$scalaPkg.List[Err], $genericType] =
              $scalaPkg.Right(${genericType.typeSymbol.asClass.module})
          """,
          q"""
            def rawConstruct(fieldValues: $scalaPkg.Seq[$scalaPkg.Any]): $genericType =
              ${genericType.typeSymbol.asClass.module}
           """)
        val impl = q"""
          $typeNameDef
          ${c.prefix}.combine(new $magCaseClassType[$typeConstructor, $genericType](
            $typeName,
            true,
            false,
            new $scalaPkg.Array(0),
            $scalaPkg.Array(..$classAnnotationTrees)
          ) {
            ..$classBody
          })
        """
        Some(impl)
      } else if (isCaseClass || isValueClass) {
        val headParamList = {
          val primaryConstructor = classType.map(_.primaryConstructor)
          val optList = primaryConstructor.flatMap(_.asMethod.typeSignature.paramLists.headOption)
          optList.map(_.map(_.asTerm))
        }

        val caseClassParameters = genericType.decls.sorted.collect(
          if (isValueClass) { case p: TermSymbol if p.isParamAccessor && p.isMethod => p }
          else { case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p }
        )

        val factoryMethod = TermName {
          if (isReadOnlyTypeclass && isValueClass) "readOnlyValueParam"
          else if (isReadOnlyTypeclass) "readOnlyParam"
          else if (isValueClass) "valueParam"
          else "param"
        }

        case class CaseParam(paramName: TermName, repeated: Boolean, typeclass: Tree, paramType: Type, ref: TermName) {

          def compile(params: TermName, idx: Int, default: Option[Tree], annotations: List[Tree]): Tree =
            q"""$params($idx) = $magnoliaPkg.Magnolia.util.$factoryMethod[$typeConstructor, $genericType, $paramType](
              ${paramName.toString.trim},
              ${if (isValueClass) q"_.$paramName" else q"$idx"},
              $repeated,
              $magnoliaPkg.CallByNeed($ref),
              ..${default.toList.map(d => q"$magnoliaPkg.CallByNeed($d)")},
              $scalaPkg.Array(..$annotations)
            )"""
        }

        val caseParamsReversed = caseClassParameters.foldLeft[List[CaseParam]](Nil) {
          (acc, param) =>
            val paramName = param.name.decodedName.toTermName
            val (repeated, paramType) = param.typeSignatureIn(genericType).resultType match {
              case TypeRef(_, `repeatedParamClass`, typeArgs) =>
                true -> appliedType(scalaSeqType, typeArgs)
              case tpe =>
                false -> tpe
            }

            acc
              .find(_.paramType =:= paramType)
              .fold {
                val path = ProductType(paramName.toString.trim, genericType.toString)
                val frame = stack.Frame(path, resultType, assignedName)
                val searchType = appliedType(typeConstructor, paramType)
                val ref = TermName(c.freshName("paramTypeclass"))
                val derivedImplicit = stack.recurse(frame, searchType, shouldCache) {
                  typeclassTree(paramType, typeConstructor, ref)
                }.fold(error, identity)
                val assigned = deferredVal(ref, searchType, derivedImplicit)
                CaseParam(paramName, repeated, assigned, paramType, ref) :: acc
              } { backRef =>
                CaseParam(paramName, repeated, q"()", paramType, backRef.ref) :: acc
              }
        }

        val caseParams = caseParamsReversed.reverse
        val paramsVal = TermName(c.freshName("parameters"))
        val preAssignments = caseParams.map(_.typeclass)

        val annotations = headParamList.getOrElse(Nil).map(annotationsOf(_))
        val assignments = if (isReadOnlyTypeclass) {
          for (((param, annList), idx) <- caseParams.zip(annotations).zipWithIndex)
            yield param.compile(paramsVal, idx, None, annList)
        } else {
          val defaults = headParamList.fold[List[Tree]](Nil) { params =>
            if (params.exists(_.isParamWithDefault)) {
              val x = TermName(c.freshName("x"))
              val A = TypeName(c.freshName("A"))
              val dummyArgs = params.filterNot(_.isParamWithDefault).map(p => q"${p.name} = $x")
              // `A <: Nothing` to avoid dead code warnings
              val dummy = c.typecheck(q"def $x[$A <: $scalaPkg.Nothing]($x: $A) = new $genericType(..$dummyArgs)")
              val defaults = dummy.collect { case sel @ Select(_, name) if name.toString.contains("$default$") => sel }.iterator
              params.map(p => if (p.isParamWithDefault) q"$scalaPkg.Some(${defaults.next()})" else q"$scalaPkg.None")
            } else {
              params.map(_ => q"$scalaPkg.None")
            }
          }

          for ((((param, default), annList), idx) <- caseParams.zip(defaults).zip(annotations).zipWithIndex)
            yield param.compile(paramsVal, idx, Some(default), annList)
        }

        val caseClassBody = if (isReadOnlyTypeclass) List(q"") else {

          val genericParams = caseParams.zipWithIndex.map { case (typeclass, idx) =>
            val arg = q"makeParam($paramsVal($idx)).asInstanceOf[${typeclass.paramType}]"
            if(typeclass.repeated) q"$arg: _*" else arg
          }

          val rawGenericParams = caseParams.zipWithIndex.map { case (typeclass, idx) =>
            val arg = q"fieldValues($idx).asInstanceOf[${typeclass.paramType}]"
            if(typeclass.repeated) q"$arg: _*" else arg
          }

          val f = TypeName(c.freshName("F"))

          val forParams = caseParams.zipWithIndex.map { case (typeclass, idx) =>
            val part = TermName(s"p$idx")
            (if(typeclass.repeated) q"$part: _*" else q"$part", fq"$part <- new _root_.mercator.Ops(makeParam($paramsVal($idx)).asInstanceOf[$f[${typeclass.paramType}]])")
          }

          val constructMonadicImpl = if (forParams.isEmpty) q"monadic.point(new $genericType())" else q"""
          for(
            ..${forParams.map(_._2)}
          ) yield new $genericType(..${forParams.map(_._1)})
        """

          val constructEitherImpl =
            if (caseParams.isEmpty) q"$scalaPkg.Right(new $genericType())"
            else {
              val eitherVals = caseParams.zipWithIndex.map {
                case (typeclass, idx) =>
                  val part = TermName(s"p$idx")
                  val pat = TermName(s"v$idx")
                  (
                    part,
                    if (typeclass.repeated) q"$pat: _*" else q"$pat",
                    q"val $part = makeParam($paramsVal($idx)).asInstanceOf[$scalaPkg.Either[Err, ${typeclass.paramType}]]",
                    pq"$scalaPkg.Right($pat)",
                  )
              }

              // DESNOTE(2019-12-05, pjrt): Due to limits on tuple sizes, and lack of <*>, we split the params
              // into a list of tuples of at most 22 in size
              val limited = eitherVals.grouped(22).toList

              q"""
           ..${eitherVals.map(_._3)}
           (..${limited.map(k => q"(..${k.map(_._1)})")}) match {
             case (..${limited.map(k => q"(..${k.map(_._4)})")}) =>
               _root_.scala.Right(new $genericType(..${eitherVals.map(_._2)}))
             case _ =>
               _root_.scala.Left(
                 $magnoliaPkg.Magnolia.util.keepLeft(..${eitherVals.map(_._1)})
               )
           }
         """
            }

            List(
            q"""
              override def construct[Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => Return): $genericType =
                new $genericType(..$genericParams)
             """,
            q"""
              def constructMonadic[$f[_], Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => $f[Return])(implicit monadic: _root_.mercator.Monadic[$f]):$f[$genericType] = {
                $constructMonadicImpl
              }
             """,
            q"""
              def constructEither[Err, PType](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => $scalaPkg.Either[Err, PType]): $scalaPkg.Either[$scalaPkg.List[Err], $genericType] = {
                $constructEitherImpl
              }
             """,
            q"""
              def rawConstruct(fieldValues: $scalaPkg.Seq[$scalaPkg.Any]): $genericType = {
                $magnoliaPkg.Magnolia.util.checkParamLengths(fieldValues, $paramsVal.length, $typeName.full)
                new $genericType(..$rawGenericParams)
              }
             """
           )
        }

        Some(
          q"""{
            ..$preAssignments
            val $paramsVal: $scalaPkg.Array[$magParamType[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.length})
            ..$assignments

            $typeNameDef

            ${c.prefix}.combine(new $magCaseClassType[$typeConstructor, $genericType](
              $typeName,
              false,
              $isValueClass,
              $paramsVal,
              $scalaPkg.Array(..$classAnnotationTrees)
            ) {
                ..$caseClassBody
            })
          }""")
      } else if (isSealedTrait) {
        checkMethod("dispatch", "sealed traits", "SealedTrait[Typeclass, _]")
        val genericSubtypes = knownSubclasses(classType.get).toList.sortBy(_.fullName)
        val subtypes = genericSubtypes.flatMap { sub =>
          val subType = sub.asType.toType // FIXME: Broken for path dependent types
          val typeParams = sub.asType.typeParams
          val typeArgs = thisType(sub).baseType(genericType.typeSymbol).typeArgs
          val mapping = (typeArgs.map(_.typeSymbol), genericType.typeArgs).zipped.toMap
          val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
          val applied = appliedType(subType.typeConstructor, newTypeArgs)
          if (applied <:< genericType) existentialAbstraction(typeParams, applied) :: Nil else Nil
        }

        if (subtypes.isEmpty) {
          error(s"could not find any direct subtypes of $typeSymbol")
        }

        val subtypesVal: TermName = TermName(c.freshName("subtypes"))

        val typeclasses = for (subType <- subtypes) yield {
          val path = CoproductType(genericType.toString)
          val frame = stack.Frame(path, resultType, assignedName)
          subType -> stack.recurse(frame, appliedType(typeConstructor, subType), shouldCache) {
            typeclassTree(subType, typeConstructor, termNames.ERROR)
          }.fold(error, identity)
        }

        val assignments = typeclasses.zipWithIndex.map {
          case ((typ, typeclass), idx) =>
            q"""$subtypesVal($idx) = $magnoliaPkg.Magnolia.util.subtype[$typeConstructor, $genericType, $typ](
            ${typeNameRec(typ)},
            $idx,
            $scalaPkg.Array(..${annotationsOf(typ.typeSymbol)}),
            _root_.magnolia.CallByNeed($typeclass),
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }

        Some(q"""{
            val $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.size})

            ..$assignments

            $typeNameDef

            ${c.prefix}.dispatch(new $magnoliaPkg.SealedTrait(
              $typeName,
              $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]],
              $scalaPkg.Array(..$classAnnotationTrees)
            )): $resultType
          }""")
      } else if (!typeSymbol.isParameter) {
        c.prefix.tree.tpe.baseClasses
          .find { cls =>
            cls.asType.toType.decl(TermName("fallback")) != NoSymbol
          }.map { _ =>
            c.warning(c.enclosingPosition, s"magnolia: using fallback derivation for $genericType")
            q"""${c.prefix}.fallback[$genericType]"""
          }
      } else None

      for (term <- result) yield q"""{
        ${deferredVal(assignedName, resultType, term)}
        $assignedName
      }"""
    }

    val genericType: Type = weakTypeOf[T]
    val searchType = appliedType(typeConstructor, genericType)
    val directlyReentrant = stack.top.exists(_.searchType =:= searchType)
    if (directlyReentrant) throw DirectlyReentrantException()

    val result = stack
      .find(searchType)
      .map(enclosingRef => q"$magnoliaPkg.Deferred[$searchType](${enclosingRef.toString})")
      .orElse(directInferImplicit(genericType, typeConstructor))

    for (tree <- result) if (debug.isDefined && genericType.toString.contains(debug.get)) {
      c.echo(c.enclosingPosition, s"Magnolia macro expansion for $genericType")
      c.echo(NoPosition, s"... = ${showCode(tree)}\n\n")
    }

    val dereferencedResult =
      if (stack.nonEmpty) result
      else for (tree <- result) yield c.untypecheck(expandDeferred.transform(tree))

    dereferencedResult.getOrElse {
      error(s"could not infer $prefixName.Typeclass for type $genericType")
    }
  }

  object util {

    /** constructs a new [[Subtype]] instance
      *
      *  This method is intended to be called only from code generated by the Magnolia macro, and
      *  should not be called directly from users' code. */
    def subtype[Tc[_], T, S <: T](name: TypeName,
                                  idx: Int,
                                  anns: Array[Any],
                                  tc: CallByNeed[Tc[S]],
                                  isType: T => Boolean,
                                  asType: T => S): Subtype[Tc, T] =
      new Subtype[Tc, T] with PartialFunction[T, S] {
        type SType = S
        def typeName: TypeName = name
        def index: Int = idx
        def typeclass: Tc[SType] = tc.value
        def cast: PartialFunction[T, SType] = this
        def isDefinedAt(t: T) = isType(t)
        def apply(t: T): SType = asType(t)
        def annotationsArray: Array[Any] = anns
        override def toString: String = s"Subtype(${typeName.full})"
      }

    def readOnlyParam[Tc[_], T, P](
                                    name: String,
                                    idx: Int,
                                    isRepeated: Boolean,
                                    typeclassParam: CallByNeed[Tc[P]],
                                    annotationsArrayParam: Array[Any]
                                  ): ReadOnlyParam[Tc, T] = new ReadOnlyParam[Tc, T] {
      type PType = P
      override def label: String = name
      override def index: Int = idx
      override def repeated: Boolean = isRepeated
      override def typeclass: Tc[P] = typeclassParam.value
      override def dereference(t: T): P = t.asInstanceOf[Product].productElement(idx).asInstanceOf[PType]
      override def annotationsArray: Array[Any] = annotationsArrayParam
    }

    def readOnlyValueParam[Tc[_], T, P](
                                         name: String,
                                         deref: T => P,
                                         isRepeated: Boolean,
                                         typeclassParam: CallByNeed[Tc[P]],
                                         annotationsArrayParam: Array[Any]
                                       ): ReadOnlyParam[Tc, T] = new ReadOnlyParam[Tc, T] {
      type PType = P
      override def label: String = name
      override def index: Int = 0
      override def repeated: Boolean = isRepeated
      override def typeclass: Tc[P] = typeclassParam.value
      override def dereference(t: T): P = deref(t)
      override def annotationsArray: Array[Any] = annotationsArrayParam
    }

    /** constructs a new [[Param]] instance
      *
      * This method is intended to be called only from code generated by the Magnolia macro, and
      * should not be called directly from users' code. */
    def param[Tc[_], T, P](name: String,
                           idx: Int,
                           isRepeated: Boolean,
                           typeclassParam: CallByNeed[Tc[P]],
                           defaultVal: CallByNeed[Option[P]],
                           annotationsArrayParam: Array[Any]
                          ): Param[Tc, T] = new Param[Tc, T] {
      type PType = P
      def label: String = name
      def index: Int = idx
      def repeated: Boolean = isRepeated
      def default: Option[PType] = defaultVal.value
      def typeclass: Tc[PType] = typeclassParam.value
      def dereference(t: T): PType = t.asInstanceOf[Product].productElement(idx).asInstanceOf[PType]
      def annotationsArray: Array[Any] = annotationsArrayParam
    }

    def valueParam[Tc[_], T, P](name: String,
                                deref: T => P,
                                isRepeated: Boolean,
                                typeclassParam: CallByNeed[Tc[P]],
                                defaultVal: CallByNeed[Option[P]],
                                annotationsArrayParam: Array[Any]
                               ): Param[Tc, T] = new Param[Tc, T] {
      type PType = P
      def label: String = name
      def index: Int = 0
      def repeated: Boolean = isRepeated
      def default: Option[PType] = defaultVal.value
      def typeclass: Tc[PType] = typeclassParam.value
      def dereference(t: T): PType = deref(t)
      def annotationsArray: Array[Any] = annotationsArrayParam
    }

    final def checkParamLengths(fieldValues: Seq[Any], paramsLength: Int, typeName: String): Unit =
      if (fieldValues.lengthCompare(paramsLength) != 0) {
        val msg = "`" + typeName + "` has " + paramsLength + " fields, not " + fieldValues.size
        throw new java.lang.IllegalArgumentException(msg)
      }

    final def keepLeft[A](values: Either[A, _]*): List[A] =
      values.toList.collect { case Left(v) => v }

  }

  private[Magnolia] def subtype[Tc[_], T, S <: T](name: TypeName,
                                idx: Int,
                                anns: Array[Any],
                                tc: CallByNeed[Tc[S]],
                                isType: T => Boolean,
                                asType: T => S): Subtype[Tc, T] = util.subtype(name, idx, anns, tc, isType, asType)

  private[Magnolia] def readOnlyParam[Tc[_], T, P](
    name: String,
    idx: Int,
    isRepeated: Boolean,
    typeclassParam: CallByNeed[Tc[P]],
    annotationsArrayParam: Array[Any]
  ): ReadOnlyParam[Tc, T] = util.readOnlyParam(name, idx, isRepeated, typeclassParam, annotationsArrayParam)

  private[Magnolia] def readOnlyValueParam[Tc[_], T, P](
    name: String,
    deref: T => P,
    isRepeated: Boolean,
    typeclassParam: CallByNeed[Tc[P]],
    annotationsArrayParam: Array[Any]
  ): ReadOnlyParam[Tc, T] = util.readOnlyValueParam(name, deref, isRepeated, typeclassParam, annotationsArrayParam)

  /** constructs a new [[Param]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  private[Magnolia] def param[Tc[_], T, P](name: String,
                         idx: Int,
                         isRepeated: Boolean,
                         typeclassParam: CallByNeed[Tc[P]],
                         defaultVal: CallByNeed[Option[P]],
                         annotationsArrayParam: Array[Any]
                        ): Param[Tc, T] = util.param(name, idx, isRepeated, typeclassParam, defaultVal, annotationsArrayParam)

  private[Magnolia] def valueParam[Tc[_], T, P](name: String,
                         deref: T => P,
                         isRepeated: Boolean,
                         typeclassParam: CallByNeed[Tc[P]],
                         defaultVal: CallByNeed[Option[P]],
                         annotationsArrayParam: Array[Any]
                        ): Param[Tc, T] = util.valueParam(name, deref, isRepeated, typeclassParam, defaultVal, annotationsArrayParam)

  private[Magnolia] final def checkParamLengths(fieldValues: Seq[Any], paramsLength: Int, typeName: String): Unit = util.checkParamLengths(fieldValues, paramsLength, typeName)

  private[Magnolia] final def keepLeft[A](values: Either[A, _]*): List[A] = util.keepLeft(values: _*)

}

private[magnolia] final case class DirectlyReentrantException()
    extends Exception("attempt to recurse directly")

@compileTimeOnly("magnolia.Deferred is used for derivation of recursive typeclasses")
object Deferred { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed abstract class TypePath(path: String) { override def toString: String = path }
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String)
      extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
      extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack[C <: whitebox.Context with Singleton] {
    private var frames = List.empty[Frame]
    private var errors = List.empty[Frame]
    private val cache = mutable.Map.empty[C#Type, C#Tree]

    def isEmpty: Boolean = frames.isEmpty
    def nonEmpty: Boolean = frames.nonEmpty
    def top: Option[Frame] = frames.headOption
    def pop(): Unit = frames = frames drop 1
    def push(frame: Frame): Unit = frames ::= frame

    def within[A](frame: Frame)(thunk: => A): A = {
      push(frame)
      try thunk finally pop()
    }

    def clear(): Unit = {
      frames = Nil
      errors = Nil
      cache.clear()
    }

    def find(searchType: C#Type): Option[C#TermName] = frames.collectFirst {
      case Frame(_, tpe, term) if tpe =:= searchType => term
    }

    def recurse[T <: C#Tree](frame: Frame, searchType: C#Type, shouldCache: C#Tree => Boolean)(
      thunk: => Either[String, C#Tree]
    ): Either[String, C#Tree] = within(frame) {
      cache.get(searchType) match {
        case Some(cached) =>
          errors = Nil
          Right(cached)
        case None =>
          thunk match {
            case failure @ Left(_) =>
              errors ::= frame
              failure
            case success @ Right(tree) =>
              if (shouldCache(tree)) cache(searchType) = tree
              errors = Nil
              success
          }
      }
    }

    def trace: (Option[Frame], List[TypePath]) = {
      val allFrames = errors reverse_::: frames
      val trace = (allFrames.drop(1), allFrames).zipped.collect {
        case (Frame(path, tp1, _), Frame(_, tp2, _))
          if !(tp1 =:= tp2) => path
      }.toList
      (allFrames.headOption, trace)
    }

    override def toString: String =
      frames.mkString("magnolia stack:\n", "\n", "\n")

    case class Frame(path: TypePath, searchType: C#Type, term: C#TermName)
  }

  object Stack {
    // Cheating to satisfy Singleton bound (which improves type inference).
    private val dummyContext: whitebox.Context = null
    private val threadLocalStack = ThreadLocal.withInitial[Stack[dummyContext.type]](() => new Stack[dummyContext.type])
    private val threadLocalWorkSet = ThreadLocal.withInitial[mutable.Set[whitebox.Context#Symbol]](() => mutable.Set.empty)

    def withContext(c: whitebox.Context)(fn: Stack[c.type] => c.Tree): c.Tree = {
      val stack = threadLocalStack.get()
      val workSet = threadLocalWorkSet.get()
      workSet += c.macroApplication.symbol
      val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
      try fn(stack.asInstanceOf[Stack[c.type]])
      finally if (depth <= 1) {
        stack.clear()
        workSet.clear()
      }
    }
  }
}

object CallByNeed { def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a) }
final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable {
  lazy val value: A = {
    val result = eval()
    eval = null
    result
  }
}
