package com.youdevise.eithervalidation

import org.specs2.Specification

import com.youdevise.eithervalidation.EitherValidationImplicits._

class EitherValidationSpec extends Specification {
  def is =
    "Examples" ^
      "Using a curried function and explicit apply method" ^
        "add(x, y)" ^
          "should return a right containing sum when both are rights" ! {
            val f = (add _).curried
            Right(f).apply(Right(2)).apply(Right(3)) must_== Right(5)
          } ^
          "should return a left containing all failures in a list when both are lefts containing lists" ! {
            val f = (add _).curried
            Right(f).apply(Left(List("a"))).apply(Left(List("b"))) must_== Left(List("a", "b"))
          } ^
          "should return a left containing all failures in a string when both are lefts containing strings" ! {
            val f = (add _).curried
            Right(f).apply(Left("a")).apply(Left("b")) must_== Left("ab")
          } ^
          bt ^
        "Person(age, name, postcode)" ^
          "should return a right containing fields when all validate" ! {
            val f = (Person.apply _).curried
            Right(f)(validAge("42"))(validName("Arthur"))(validPostcode("1234")) must_== Right(Person(42, "Arthur", "1234"))
          } ^
          "should return a left containing all failures" ! {
            val f = (Person.apply _).curried
            Right(f)(validAge("150"))(validName("dude"))(validPostcode("a1")) must_== Left(List("Age must be less than 130", "Name must begin with a capital letter", "Postcode must be 4 digits"))
          } ^
          "should return a left containing all failures as a string, when failures are strings" ! {
            val f = (Person.apply _).curried
            Right(f)(validAge2("150"))(validName2("dude"))(validPostcode2("a1")) must_== Left("Age must be less than 130\nName must begin with a capital letter\nPostcode must be 4 digits\n")
          } ^
    end

  def add(x: Int, y: Int): Int = x + y

  case class Person(age: Int, name: String, postcode: String)

  def validAge(s: String): Either[List[String], Int] =
    try {
      val n = s.toInt
      if (n < 0)
        Left(List("Age must be greater than 0"))
      else if (n > 130)
        Left(List("Age must be less than 130"))
      else
        Right(n)
    } catch {
      case e => Left(List(e.toString))
    }

  def validName(s: String): Either[List[String], String] =
    if (s.headOption exists (_.isUpper))
      Right(s)
    else
      Left(List("Name must begin with a capital letter"))

  def validPostcode(s: String): Either[List[String], String] =
    if (s.length == 4 && s.forall(_.isDigit))
      Right(s)
    else
      Left(List("Postcode must be 4 digits"))

  def validAge2(s: String): Either[String, Int] =
    try {
      val n = s.toInt
      if (n < 0)
        Left("Age must be greater than 0\n")
      else if (n > 130)
        Left("Age must be less than 130\n")
      else
        Right(n)
    } catch {
      case e => Left(e.toString)
    }

  def validName2(s: String): Either[String, String] =
    if (s.headOption exists (_.isUpper))
      Right(s)
    else
      Left("Name must begin with a capital letter\n")

  def validPostcode2(s: String): Either[String, String] =
    if (s.length == 4 && s.forall(_.isDigit))
      Right(s)
    else
      Left("Postcode must be 4 digits\n")
}

