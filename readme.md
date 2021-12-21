![Magnolia](https://github.com/softwaremill/magnolia/raw/scala2/banner.jpg)

[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/softwaremill/magnolia)
[<img src="https://img.shields.io/maven-central/v/com.softwaremill.magnolia1_2/magnolia_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.softwaremill.magnolia1_2/magnolia_2.12)

# Magnolia

__Magnolia__ is a generic macro for automatic materialization of typeclasses for datatypes composed from *case classes* 
(products) and *sealed traits* (coproducts). It supports recursively-defined datatypes out-of-the-box, and incurs 
no significant time-penalty during compilation. If derivation fails, error messages are detailed and informative.

## Features

 - derives typeclasses for case classes, case objects and sealed traits
 - offers a lightweight, non-macro syntax for writing derivations
 - works with recursive and mutually-recursive definitions
 - supports parameterized ADTs (GADTs), including in recursive types
 - supports typeclasses whose generic type parameter is used in either
   covariant and contravariant positions
 - caches implicit searches for compile-time efficiency
 - prints an error stack to help debugging when derivation fails
 - provides access to case class default parameter values
 - offers predictable resolution of prioritized implicits
 - does not require additional type annotations, like `Lazy[T]`


## Getting Started

Given an ADT such as,
```scala
sealed trait Tree[+T]
case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[+T](value: T) extends Tree[T]
```
and provided an implicit instance of `Show[Int]` is in scope, and a Magnolia
derivation for the `Show` typeclass has been provided, we can
automatically derive an implicit typeclass instance of `Show[Tree[Int]]`
on-demand, like so,
```scala
Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show
```
Typeclass authors may provide Magnolia derivations in the Typeclass's companion
object, but it is easy to create your own.

The derivation typeclass for a `Show` typeclass might look like this:
```scala
import language.experimental.macros, magnolia1._

object ShowDerivation {
  type Typeclass[T] = Show[T]
  
  def join[T](ctx: CaseClass[Show, T]): Show[T] = new Show[T] {
    def show(value: T): String = ctx.parameters.map { p =>
      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
    }.mkString("{", ",", "}")
  }

  def split[T](ctx: SealedTrait[Show, T]): Show[T] =
    new Show[T] {
      def show(value: T): String = ctx.split(value) { sub =>
        sub.typeclass.show(sub.cast(value))
      }
    }

  implicit def gen[T]: Show[T] = macro Magnolia.gen[T]
}
```

The `gen` method will attempt to construct a typeclass for the type passed to
it. Importing `ShowDerivation.gen` from the example above will make generic
derivation for `Show` typeclasses available in the scope of the import. The
`macro Magnolia.gen[T]` binding must be made in a static object, and the type
constructor, `Typeclass`, and the methods `join` and `split` must be
defined in the same object.

If you control the typeclass you are deriving for, the companion object of the
typeclass makes a good choice for providing the implicit derivation methods
described above.

## Debugging

Deriving typeclasses is not always guaranteed to succeed, though. Many
datatypes are complex and deeply-nested, and failure to derive a typeclass for
a single parameter in one of the leaf nodes will cause the entire tree to fail.

Magnolia tries to be informative about why failures occur, by providing a
"stack trace" showing the path to the type which could not be derived.

For example, when attempting to derive a `Show` instance for `Entity`, given
the following hypothetical datatypes,

```scala
sealed trait Entity
case class Person(name: String, address: Address) extends Entity
case class Organization(name: String, contacts: Set[Person]) extends Entity
case class Address(lines: List[String], country: Country)
case class Country(name: String, code: String, salesTax: Boolean)
```
the absence, for example, of a `Show[Boolean]` typeclass instance would cause
derivation to fail, but the reason might not be obvious, so instead, Magnolia
will report the following compile error:

```scala
could not derive Show instance for type Boolean
    in parameter 'salesTax' of product type Country
    in parameter 'country' of product type Address
    in parameter 'address' of product type Person
    in chained implicit of type Set[Person]
    in parameter 'contacts' of product type Organization
    in coproduct type Entity
```

This "derivation stack trace" will only be displayed when invoking a derivation
method, e.g. `Show.gen[Entity]`, directly. When the method is invoked through
implicit search, to reduce spurious error messages (when Magnolia's derivation
fails, but implicit search still finds a valid implicit) the errors are not
shown.

## Availability

For Scala 2:

A binary is available on Maven Central as `com.softwaremill.magnolia1_2:magnolia_<scala-version>:1.0.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:

```
libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.0.0"
```

For Scala 3, see the [scala3 branch](https://github.com/softwaremill/magnolia/tree/scala3).

## Package and artifact naming, versioning

The main magnolia package is `magnolia1`, so that magnolia 1.x can be used alongside magnolia 0.17 (which are binary-incompatible).
Future major releases of magnolia can change the package name for the same reason.

The group id for magnolia follows the naming scheme: `com.softwaremill.magnolia[major version]_[scala major version]`.
The scala major version suffix is necessary to allow evolving and publishing versions for Scala 2 & Scala 3 independently.
The magnolia major version is included for consistency with the package name, and so that future major releases may be
used alongside this release.

## Contributing

Contributors to Magnolia are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/softwaremill/magnolia/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

## Credits

Magnolia was originally designed and developed by [Jon Pretty](https://github.com/propensive), and is currently
maintained by [SoftwareMill](https://softwaremill.com).

## License

Magnolia is made available under the [Apache 2.0 License](/license.md).
