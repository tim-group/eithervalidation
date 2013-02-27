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
 * In other words, by placing a function X => Y into a Right, you have lifted
 * it into an Either context. You can then apply it to another Either. If your
 * function f takes multiple parameters, use the curried form of the function
 * `(f _).curried`, and chain the calls to the subsequent parameters, as
 * demonstrated below.
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
 * One difference here is that, instead of requiring the caller to use symbol
 * method names and to reverse the order of parameters, we are instead
 * enriching Either with an #apply method, so that the "applicative" nature
 * becomes natural and discoverable in the normal idioms of Scala. It's about
 * "applying", after all, so why not use #apply.
 *
 * Another difference is that we don't publish a Semigroup typeclass to the
 * top-level namespace. At the moment we, add a private typeclass in order to
 * support appending to String and Array as well as all TraversableLike
 * collection classes, but this is only necessary due to SI-3346 [1][2][3][4],
 * and will likely change in the future -- we'll eliminate the typeclass, and
 * simply match any type in the left of the either which is viewable as a
 * TraversableLike without additional ceremony.
 *
 * This library is written to support Scala back to at least 2.9.1. In 2.10
 * and beyond, there have been some simplifications of the implicit typing for
 * collections to limit the appearance of CanBuildFrom [5].
 *
 *   [1] https://issues.scala-lang.org/browse/SI-3346
 *   [2] http://stackoverflow.com/questions/8472654/how-to-enrich-scala-collections-with-my-own-generic-map-the-right-way/8477943#8477943
 *   [3] http://www.scala-lang.org/node/11889
 *   [4] http://yz.mit.edu/wp/true-scala-complexity/
 *   [5] http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to-scala-collections
 */

/** Wraps an Either which has a TraversableLike type in the left, and a function in the right */
case class EitherValidation[E : EitherValidation.Semigroup, X, Y](either1: Either[E, X => Y]) {
  def apply(either2: Either[E, X]): Either[E, Y] = {
    val semigroup = implicitly[EitherValidation.Semigroup[E]]
    (either1, either2) match {
      case (Left(e1), Left(e2)) => Left(semigroup.append(e1, e2))
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e2)) => Left(e2)
      case (Right(f), Right(x)) => Right(f(x))
    }
  }
}

/** To support just saying `Right(f)(x)(y)`, wraps an Either which has Nothing in the left, and a function in the right */
case class EitherWithLeftNothingValidation[X, Y](either1: Either[Nothing, X => Y]) {
  def apply[E](either2: Either[E, X]): Either[E, Y] = {
    (either1, either2) match {
      case (_,        Left(e2)) => Left(e2)
      case (Right(f), Right(b)) => Right(f(b))
    }
  }
}

object EitherValidation {
  /** A trait to describe anything that can be appended, for the Left of an EitherValidation */
  trait Semigroup[E] {
    def append(l: E, r: => E): E
  }

  /** Import these implicits into a scope to treat qualified Eithers as EitherValidations */
  object Implicits {
    /** We know how to append anything viewable as a TraversableLike */
    implicit def TraversableLike2EitherValidationSemigroup[E1, T[E2] <: TraversableLike[E2, T[E2]]](implicit cbf: CanBuildFrom[T[E1], E1, T[E1]]): EitherValidation.Semigroup[T[E1]] = new EitherValidation.Semigroup[T[E1]] {
      override def append(l: T[E1], r: => T[E1]): T[E1] = l ++ r
    }

    /** Only necessary because Scala doesn't realize that String is viewable as TraversableLike, see SI-3346 */
    implicit object StringEitherValidationSemigroup extends EitherValidation.Semigroup[String] {
      def append(l: String, r: => String) = l ++ r
    }

    implicit def EitherWithLeftNothing2EitherWithLeftNothingValidation[E, X, Y](e: Either[Nothing, X => Y]): EitherWithLeftNothingValidation[X, Y] = EitherWithLeftNothingValidation(e)
    implicit def Either2EitherValidation[E : EitherValidation.Semigroup, X, Y](e: Either[E, X => Y]): EitherValidation[E, X, Y] = EitherValidation(e)
  }
}