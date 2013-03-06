import sbt._
import Keys._

object EitherValidationBuild extends Build {

  lazy val main = Project(
    id        = "main",
    base      = file( "." )
  )

  // Sometimes libraries, like specs2, have a latest version for older Scala versions
  val scala290 = """2\.9\.0.*""".r
  val scala29 = """2\.9.*""".r

  def specs2Dependencies(scalaVersion: String) = {
    scalaVersion match {
      case scala290() => Seq(
        "org.specs2" %% "specs2" % "1.7.1" % "test"
      )
      case scala29() => Seq(
        "org.specs2" %% "specs2" % "1.12.3" % "test"
      )
      case _ => Seq(
        "org.specs2" % "specs2_2.10" % "1.14" % "test"// current version for Scala 2.10.x
      )
    }
  }

  def scalaCompilerDependency(scalaVersion: String) = {
    Seq("org.scala-lang" % "scala-compiler" % scalaVersion % "test")
  }
}