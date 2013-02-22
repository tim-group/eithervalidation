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
 * NOTE: This is written to support Scala back to at least 2.9.1. In 2.10 and
 *   beyond, they have simplified some of the implicit typing for the
 *   collections to limit CanBuildFrom's appearance, as explained here:
 *     http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to-scala-collections
 */

/** Wraps an Either which has a TraversableLike type in the left, and a function in the right */
case class EitherValidation[E : Semigroup, X, Y](either1: Either[E, X => Y]) {
  def apply(either2: Either[E, X]): Either[E, Y] = {
    val semigroup = implicitly[Semigroup[E]]
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

trait Semigroup[E] {
  def append(l: E, r: => E): E
}

/** Import these implicits into a scope to treat Eithers as validations */
object EitherValidationImplicits {

  implicit object StringSemigroup extends Semigroup[String] {
    def append(l: String, r: => String) = l + r
  }

  implicit def eitherWithLeftNothing2eitherWithLeftNothingValidation[E, X, Y](e: Either[Nothing, X => Y]): EitherWithLeftNothingValidation[X, Y] = EitherWithLeftNothingValidation(e)
  implicit def either2eitherValidation[E : Semigroup, X, Y](e: Either[E, X => Y]): EitherValidation[E, X, Y] = EitherValidation(e)

  implicit def TraversableSemigroup[E1, T[E2] <: TraversableLike[E2, T[E2]]](implicit cbf: CanBuildFrom[T[E1], E1, T[E1]]): Semigroup[T[E1]] = new Semigroup[T[E1]] {
    override def append(s1: T[E1], s2: => T[E1]): T[E1] = {
      s1 ++ s2
    }
  }
}
