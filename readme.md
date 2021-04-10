[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/magnolia/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/magnolia/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/magnolia)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.magnolia:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.magnolia:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/magnolia_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/magnolia_2.12)
[<img src="https://vent.dev/badge/propensive/magnolia" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Magnolia

__Magnolia__ is a generic macro for automatic materialization of typeclasses for datatypes composed from product types (e.g. case classes) and coproduct types (e.g. enums). It supports recursively-defined datatypes out-of-the-box, and incurs no significant time-penalty during compilation.

## Features

 - derives typeclasses for case classes, case objects and sealed traits
 - offers a lightweight syntax for writing derivations without needing to understand complex parts of Scala
 - builds upon Scala 3's built-in generic derivation
 - works with recursive and mutually-recursive definitions
 - supports parameterized ADTs (GADTs), including those in recursive types
 - supports typeclasses whose generic type parameter is used in either covariant and contravariant positions


## Getting Started

Given an ADT such as,
```scala
enum Tree[+T]:
  case class Branch(left: Tree[T], right: Tree[T])
  case class Leaf(value: T)
```
and provided an given instance of `Show[Int]` is in scope, and a Magnolia derivation for the `Show` typeclass
has been provided, we can automatically derive given typeclass instances of `Show[Tree[Int]]` on-demand, like
so,
```scala
Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show
```
Typeclass authors may provide Magnolia derivations in the typeclass's companion object, but it is easy to create
your own.

The definition of a `Show` typeclass with generic derivation defined with Magnolia might look like this:
```scala
import magnolia.*

trait Show[T]:
  def show(value: T): String

object Show extends Derivation[Show]:
  def join[T](ctx: CaseClass[Show, T]): Show[T] =
    ctx.params.map { p =>
      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
    }.mkString("{", ",", "}")

  override def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.dispatch(value) { sub => sub.typeclass.show(sub.cast(value))
```

The `Derivation` trait provides a `derived` method which will attempt to construct a corresponding typeclass
instance for the type passed to it. Importing `Show.derived` as defined in the example above will make generic
derivation for `Show` typeclasses available in the scope of the import.

While any object may be used to define a derivation, if you control the typeclass you are deriving for, the
companion object of the typeclass is the obvious choice since it generic derivations for that typeclass will
be automatically available for consideration during contextual search.

## Limitations

Magnolia is not currently able to access default values for case class parameters.

## Status

Magnolia is classified as __maturescent__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Magnolia&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/magnolia`.
```
fury layer clone -i propensive/magnolia
```
or imported into an existing layer with,
```
fury layer import -i propensive/magnolia
```
A binary is available on Maven Central as `com.propensive:magnolia_<scala-version>:2.0.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "magnolia" % "2.0.0"
```

## Contributing

Contributors to Magnolia are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/magnolia/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Magnolia easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Magnolia was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Magnolia is copyright &copy; 2018-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
