package com.youdevise.eithervalidation

import org.specs2.Specification

class CompileErrorsSpec extends Specification {

  // Lifted from https://github.com/jorgeortiz85/linter/blob/master/src/test/scala/LinterPluginTest.scala
  class Compiler {
    import java.io.{PrintWriter, StringWriter}
    import scala.io.Source
    import scala.tools.nsc.{Global, Settings}
    import scala.tools.nsc.interpreter.{IMain, Results}
    import scala.tools.nsc.reporters.Reporter

    private val settings = new Settings
    val loader = manifest[EitherValidation$].erasure.getClassLoader
    settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
    settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
    settings.deprecation.value = true // enable detailed deprecation warnings
    settings.unchecked.value = true // enable detailed unchecked warnings
    settings.Xwarnfatal.value = true // warnings cause compile failures too

    val stringWriter = new StringWriter()

    // This is deprecated in 2.9.x, but we need to use it for compatibility with 2.8.x
    private val interpreter = new IMain(settings, new PrintWriter(stringWriter)) {
      override protected def newCompiler(settings: Settings, reporter: Reporter) = {
        settings.outputDirs setSingleOutput virtualDirectory
        new Global(settings, reporter)
      }
    }

    def compile(code: String): Option[String] = {
      stringWriter.getBuffer.delete(0, stringWriter.getBuffer.length)
      val thunked = "() => { %s }".format(code)
      interpreter.interpret(thunked) match {
        case Results.Success => None
        case Results.Error => Some(stringWriter.toString)
        case Results.Incomplete => throw new Exception("Incomplete code snippet")
      }
    }
  }

  val compiler = new Compiler

  def is =
    "throws a type checking error when trying to apply non-traversable types to an EitherValidation" ! {
      compiler.compile(
        """import com.youdevise.eithervalidation.EitherValidation.Implicits._
          | val valueOfNonTraversableType = 1
          | Right({(i: Int, j: Int) => i + j})(Left(valueOfNonTraversableType), Left(valueOfNonTraversableType))""".stripMargin
      ) must beSome.which(_.contains("The applied Left must be a Traversable like List or convertible to a Traversable like String. You tried to apply a Left[Int]"))
    }
}
