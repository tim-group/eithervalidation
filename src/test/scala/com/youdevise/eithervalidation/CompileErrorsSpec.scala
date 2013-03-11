package com.youdevise.eithervalidation

import org.specs2.Specification

class CompileErrorsSpec extends Specification {

  // Lifted from https://github.com/jorgeortiz85/linter/blob/master/src/test/scala/LinterPluginTest.scala
  class Compiler {
    import java.io.{PrintWriter, StringWriter}
    import scala.io.Source
    import scala.tools.nsc.Settings
    import scala.tools.nsc.interpreter.{IMain, Results}

    private val settings = new Settings
    val loader = classOf[EitherValidation[List[Int], Nothing, Nothing]].getClassLoader
    settings.classpath.value = Source.fromURL(loader.getResource("app.class.path")).mkString
    settings.bootclasspath.append(Source.fromURL(loader.getResource("boot.class.path")).mkString)
    settings.deprecation.value = true // enable detailed deprecation warnings
    settings.unchecked.value = true // enable detailed unchecked warnings
    settings.Xwarnfatal.value = true // warnings cause compile failures too

    val stringWriter = new StringWriter()

    private val interpreter = new IMain(settings, new PrintWriter(stringWriter))

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
