<img src="/doc/images/200x200.png" align="right">

# Contributing to Magnolia

Firstly, thank you for taking an interesting in contributing! Magnolia is an open-source project, and welcomes
contributions in the form of feature code, bug reports & fixes, tests, feature suggestions and anything else
which may help to make it better software.

## Developing with Fury

Magnolia is developed using [Fury](https://github.com/propensive/fury), which may be installed by running the
launcher script in the root directory, like so:
```
./fury system install
```

If you do not wish to _install_ Fury, the same script may be used to _run_ Fury without modifying any of your
system settings.

### Compiling

The simplest way to compile Magnolia is to run,
```
fury
```
however, useful variations include,
- `fury -w`: watch for source-file changes, and recompile as necessary
- `fury build console -I`: compile and launch the Scala REPL, ignoring any compile errors
- `fury -F -f <dir>`: compile and save a fat JAR file
- `fury test`: compile and run the test suite

Fury is not yet stable, so please [report any issues](https://github.com/propensive/fury/issues/) relating to
Fury.

We recommend using [Visual Studio Code](https://code.visualstudio.com/) with the
[Scala Metals extension](https://marketplace.visualstudio.com/items?itemName=scalameta.metals) for developing.

## Before Starting

It&rsquo;s a good idea to [discuss](https://riot.im/app/#/room/#propensive.magnolia:matrix.org) potential changes
with one of the maintainers before starting work. Although efforts are made to document future development work
using the [issue tracker](/issues), it will not always be up-to-date, and the maintainers may have useful
information to share. The worst-case scenario would be for a contributor to spend a large amount of time
producing a PR, only for it to be rejected by the maintainers for not fitting with their ideas. A quick
conversation before starting work can save a lot of time.

If a response is not forthcoming in the [Riot chatroom](https://riot.im/app/#/room/#propensive.magnolia:matrix.org),
contacting one of the project maintainers directly _but publicly_ may help. Please __do not__ contact the
maintainers privately, as it misses a good opportunity to share knowledge with a wide audience. Jon Pretty can
usually be contacted [on Twitter](https://twitter.com/propensive).

All development work&mdash;whether bugfixing or implementing new features&mdash;should have a corresponding
issue before work starts. If you have commit rights to the `propensive/magnolia` repository, please only push to
branches which named after the issue number, prefixed with `issue/`, for example, `issue/423`.

## Contribution standards

Pull requests should try to follow the coding style of existing code in the repository. They are unlikely to be
rejected on grounds of formatting, except in extreme cases. Magnolia does not use automatic code-formatting
because it has proven to produce unreliable results (and furthermore, hand-formatting is not particularly
laborious).

Unfortunately an official style guide does not yet exist.

Pull requests should have at least one review before being merged. When opening a PR, contributors are welcome
to suggest a reviewer. Pull requests should be left in _draft_ mode until they are believed to be ready for
review.

For code contributions, we prefer pull requests with corresponding tests. But we should not _let perfect be
the enemy of the good_. Changes which break existing tests, however, are likely to be rejected during review.

## Reporting issues

New issues are welcome, both as bug reports and feature suggestions. It is generally true that more detail is
preferable, and the clearest and most detailed reports will most likely be addressed sooner, but a short report
from a busy developer is still preferred over a bug we never hear about. We will ask for more detail in triage
if it&rsquo;s needed.

## Conduct

Contributors and other participants in online discussions are expected to be polite and to nurture a pleasant
development environment for all Magnolia&rsquo;s users. Propensive O&Uuml; reserves the right to censure
and&mdash;in extreme cases&mdash;ban users whose behavior, in their opinion, is detrimental toward this goal.
