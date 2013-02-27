package com.youdevise.eithervalidation

import collection.TraversableLike
import collection.generic.CanBuildFrom

/**
 * Enriches Either to do the business of an Applicative Functor: allow you to
 * apply a function X => Y inside an Either to a parameter inside an Either,
 * returning another Either.
 *
 * If both the function Either and the parameter Either are Rights, returns
 * the result of applying the function to the parameter in a Right. But, if
 * any were Lefts, returns a Left all the left values concatenated using ++.
 *
 * In other words, by placing a function A => B into a Right, you have lifted
 * it into an Either context. You can then apply it to another Either. If your
 * function f takes multiple parameters (A, B) => C, you simply apply it to
 * the same number of Eithers.
 *
 * For example, in the REPL:
 *
 *     scala> def add(x: Int, y: Int): Int = x + y
 *     add: (x: Int, y: Int)Int
 *
 *     scala> add(1, 2)
 *     res0: Int = 3
 *
 *     scala> Right(add _)(Right(1), Right(2))
 *     res1: Either[Nothing, Int] = Right(3)
 *
 *     scala> Right(add _)(Left(List("error!")), Right(2))
 *     res2: Either[String, Int] = Left(List("error!"))
 *
 *     scala> Right(add _)(Left(List("error 1!")), Left(List("error 2!")))
 *     res3: Either[String, Int] = Left(List("error 1!", "error 2!"))
 *
 * You may also choose to use the curried form of the function `(f _).curried`
 * directly and chain the calls to the subsequent parameters.
 *
 * For example, in the REPL:
 *
 *     scala> def add(x: Int, y: Int): Int = x + y
 *     add: (x: Int, y: Int)Int
 *
 *     scala> add(1, 2)
 *     res0: Int = 3
 *
 *     scala> val curriedAdd = (add _).curried
 *     curriedAdd: Int => Int => Int = <function1>
 *
 *     scala> Right(curriedAdd)(Right(1))(Right(2))
 *     res1: Either[Nothing, Int] = Right(3)
 *
 *     scala> Right(curriedAdd)(Left(List("error!")))(Right(2))
 *     res2: Either[String, Int] = Left(List("error!"))
 *
 *     scala> Right(curriedAdd)(Left(List("error 1!")))(Left(List("error 2!")))
 *     res3: Either[String, Int] = Left(List("error 1!", "error 2!"))
 *
 * This is a *lot* like the approach taken in Scalaz, as described here:
 *   http://applicative-errors-scala.googlecode.com/svn/artifacts/0.6/pdf/index.pdf
 *
 * One difference here is that, instead of requiring the caller to curry the
 * function, use a specific Validation class instead of Either, invoke Haskell-
 * derived symbolic operator names, and reverse the order of the function's
 * parameters, we are instead enriching Either with an #apply method, so that
 * the "applicative" nature becomes natural and discoverable in the normal
 * idioms of Scala. It's about "applying", after all, so why not use #apply?
 *
 * Another difference is that we don't publish a Semigroup typeclass to the
 * top-level namespace. At the moment we add a local Semigroup typeclass in
 * order to support appending to String and Array as well as to all
 * TraversableLike collection classes, but this is only necessary due to
 * SI-3346 [1][2][3][4], and will likely change in the future. We'll eliminate
 * the typeclass, and simply match any type in the left of the Either which is
 * viewable as a TraversableLike, without the additional ceremony.
 *
 * This library is written to support Scala back to at least 2.9.1. In 2.10
 * and beyond, there appear to have been some simplifications of the implicit
 * typing for collections to limit the appearance of CanBuildFrom [5], which we
 * are not using.
 *
 *   [1] https://issues.scala-lang.org/browse/SI-3346
 *   [2] http://stackoverflow.com/questions/8472654/how-to-enrich-scala-collections-with-my-own-generic-map-the-right-way/8477943#8477943
 *   [3] http://www.scala-lang.org/node/11889
 *   [4] http://yz.mit.edu/wp/true-scala-complexity/
 *   [5] http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to-scala-collections
 */

/** Wraps an Either which has a type viewable as TraversableLike in the Left, and a Function1 in the Right */
case class EitherValidation[Left : EitherValidation.Semigroup, A, B](e1: Either[Left, A => B]) {
  def apply(e2: Either[Left, A]): Either[Left, B] = {
    val semigroup = implicitly[EitherValidation.Semigroup[Left]]
    (e1, e2) match {
      case (Left(l1), Left(l2)) => Left(semigroup.append(l1, l2))
      case (Left(l1), Right(_)) => Left(l1)
      case (Right(_), Left(l2)) => Left(l2)
      case (Right(f), Right(x)) => Right(f(x))
    }
  }
}

/** To support just saying `Right(f)(x)(y)`, wraps an Either which has Nothing in the Left, and a Function1 in the Right */
case class EitherWithLeftNothingValidation1[A, B](e1: Either[Nothing, A => B]) {
  def apply[Left](e2: Either[Left, A]): Either[Left, B] = {
    (e1, e2) match {
      case (_,        Left(l2)) => Left(l2)
      case (Right(f), Right(x)) => Right(f(x))
    }
  }
}

/** To support just saying `Right(f)(x, y)`, wraps an Either which has Nothing in the Left, and a Function2 in the Right */
case class EitherWithLeftNothingValidation2[A, B, C](e1: Either[Nothing, (A, B) => C]) {
  def apply[Left : EitherValidation.Semigroup](e2: Either[Left, A], e3: Either[Left, B]): Either[Left, C] = {
    import EitherValidation.Implicits._
    e1.right.map(_.curried)(e2)(e3)
  }
}

/** To support just saying `Right(f)(x, y, z)`, wraps an Either which has Nothing in the Left, and a Function3 in the Right */
case class EitherWithLeftNothingValidation3[A, B, C, D](e1: Either[Nothing, (A, B, C) => D]) {
  def apply[Left : EitherValidation.Semigroup](e2: Either[Left, A], e3: Either[Left, B], e4: Either[Left, C]): Either[Left, D] = {
    import EitherValidation.Implicits._
    e1.right.map(_.curried)(e2)(e3)(e4)
  }
}

object EitherValidation {
  /** A trait to describe anything that can be appended, for the Left of an EitherValidation */
  trait Semigroup[E] {
    def append(l: E, r: => E): E
    def append(l: Nothing, r: => E): E = r
    def append(l: E, r: Nothing): E = l
  }

  /** Import these implicits into a scope to treat qualified Eithers as EitherValidations */
  object Implicits {
    /** We know how to append anything viewable as a TraversableLike, so that's our Semigroup */
    implicit def TraversableLike2EitherValidationSemigroup[E, CC <% TraversableLike[E, CC] : ({ type L[A] = CanBuildFrom[A, E, A] })#L]: EitherValidation.Semigroup[CC] = new EitherValidation.Semigroup[CC] {
      override def append(l: CC, r: => CC): CC = l ++ r
    }

    /** We know that we don't need to append Nothing on both sides */
    implicit def Nothing2EitherValidationSemigroup = new EitherValidation.Semigroup[Nothing] {
      override def append(l: Nothing, r: => Nothing): Nothing = l
    }

    /** We know how to treat a `Right(f)`, with Nothing in the Left, as an EitherValidation */
    implicit def EitherWithLeftNothing1EitherWithLeftNothingValidation[A, B](e: Either[Nothing, A => B]): EitherWithLeftNothingValidation1[A, B] = EitherWithLeftNothingValidation1(e)
    implicit def EitherWithLeftNothing2EitherWithLeftNothingValidation[A, B, C](e: Either[Nothing, (A, B) => C]): EitherWithLeftNothingValidation2[A, B, C] = EitherWithLeftNothingValidation2(e)
    implicit def EitherWithLeftNothing3EitherWithLeftNothingValidation[A, B, C, D](e: Either[Nothing, (A, B, C) => D]): EitherWithLeftNothingValidation3[A, B, C, D] = EitherWithLeftNothingValidation3(e)

    /** We know how to treat an Either with anything having a Semigroup typeclass instance in the Left,
      * and a Function1 in the Right, as an EitherValidation */
    implicit def Either2EitherValidation[Left : EitherValidation.Semigroup, A, B](e: Either[Left, A => B]): EitherValidation[Left, A, B] = EitherValidation(e)
  }
}