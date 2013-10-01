EitherValidation
================
[![Build Status](https://travis-ci.org/youdevise/eithervalidation.png)](https://travis-ci.org/youdevise/eithervalidation)

This is a single file approach to treating Scala's standard library Either class
like a Scalaz Validation. It provides a mechanism for treating Either as an
applicative functor by enriching it with an #apply method when the Right type
is a function.

The motivation is to be able to use the lessons from Tony Morris's paper
[_Applicative Programming, Disjoint Unions, Semigroups and Non-breaking Error Handling_](http://applicative-errors-scala.googlecode.com/svn/artifacts/0.6/pdf/index.pdf),
but using only the standard library's Either class, and using the #apply method
for the applicative functor invocation.

As an example, consider the `Person` case class presented in the above
paper, and the three validations applied to the parameters to `Person`:
`validAge`, `validName`, and `validPostcode`. In this library, these
function would return the standard library `Either` class instead of
the Scalaz `Validation`, and main would look something like this:

```scala
def main(args: Array[String]) {
  if(args.length < 3)
    println("Need at least three arguments")
  else {
    // Notice the natural syntax to lift function into Either, and then apply it to Eithers
    Right(Person)(validAge(args(0)), validName(args(1)), validPostcode(args(2))) match {
      case Right(p) => println("We have a person: " + p)
      case Left(e) => e foreach println
    }
  }
}
```

Compare to the original version which uses Haskell-like syntax, reverses order of
parameters, and requires Scalaz's Validation class instead of the standard library's
Either class:

```scala
def main(args: Array[String]) {
  if(args.length < 3)
    println("Need at least three arguments")
  else {
    val f = (Person(_, _, _)).curried
    val age = validAge(args(0))
    val name = validName(args(1))
    val postcode = validPostcode(args(2))
    postcode <<*>> (name <<*>> (age map f)) match {
      case Success(p) => println("We have a person: " + p)
      case Failure(e) => e foreach println
    }
  }
}
```

How to add as a dependency to your Play 2.x Project
=====================================================================
To add this library to your Play 2.x project, you can follow
[these instructions](https://github.com/playframework/Play20/wiki/SBTDependencies)
to add a dependency directly on a tagged version in this git repository.

For example, to add a dependency on EitherValidations v1.0.1 to a Play project:

```scala
// project/Build.scala
import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {
  val appName = "YourAppName"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.scalaquery" %% "scalaquery" % "0.10.0-M1",
    // artifact dependencies...
  )

  val gitDependencies: Seq[ClasspathDep[ProjectReference]] = Seq(
    RootProject(uri("git://github.com/youdevise/eithervalidation.git#v1.0.1"))
  )

  // ...

  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).dependsOn(gitDependencies : _*)
}
```

(still looking for instructions on how to do this from a regular sbt project)


How to Test w/ sbt and Edit w/ IntelliJ
=======================================

 1. Install `sbt`, -OR- create a symlink to the `sbt` installed as part of `play`

    ```bash
    # I have this as ~/bin/sbt, and the ~/bin directory is in my PATH
    SBT_DIR="/opt/play/framework/sbt"
    java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar $SBT_DIR/sbt-launch.jar "$@"
    ```

 2. Run tests

    ```bash
    sbt test
    ```

 3. Generate IntelliJ project

    ```bash
    sbt gen-idea
    ```


Status
======
All tests are passing on Scala versions back to 2.9.0-1. 
See [our Travis-CI config](.travis.yml) for all the versions that are currently tested.


Contributing
============
Please feel free to fork this project and make pull requests. Any change to code
must come with tests, of course.
