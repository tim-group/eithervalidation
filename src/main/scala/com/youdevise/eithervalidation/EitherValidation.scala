package com.youdevise.eithervalidation

import collection.TraversableLike

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
 * Another difference is that do not add an implicit SemiGroup[A] to
 * supply the appending strategy for type A. Instead, we are requiring that
 * the error collection type be a TraversableLike, and thus provide the ++
 * method for appending.
 *
 * NOTE: This is written to support Scala back to at least 2.9.1. In 2.10 and
 *   beyond, they have simplified some of the implicit typing for the
 *   collections to limit CanBuildFrom's appearance, as explained here:
 *     http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to-scala-collections
 *
 * TODO:
 *   * It currently works if you explicitly call #apply, but applying directly fails
 *     to compile when there is more than one curried parameter. This is because
 *     scala interprets the second application as the implicit CanBuildFrom parameter
 *     below. Therefore, gotta find another way to do it.
 *
 *   * It currently works if the error type is List[_], but it is not compiling when
 *     String is used. Need to adjust the type bounds to pick up String, as I know that
 *     for example "a" ++ "b" == "ab", so it should be workable.
 */

/** Wraps an Either which has a TraversableLike type in the left, and a function in the right */
case class EitherValidation[E, T[E] <: TraversableLike[E, T[E]], X, Y](either1: Either[T[E], X => Y]) {
  import collection.generic.CanBuildFrom
  def apply(either2: Either[T[E], X])(implicit cbf: CanBuildFrom[T[E], E, T[E]]): Either[T[E], Y] = {
    (either1, either2) match {
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e2)) => Left(e2)
      case (Right(f), Right(x)) => Right(f(x))
    }
  }
}

/** To support just saying `Right(f)(x)(y)`, wraps an Either with Nothing in the left, and a function in the right */
case class EitherWithLeftNothingValidation[X, Y](either1: Either[Nothing, X => Y]) {
  def apply[E](either2: Either[E, X]): Either[E, Y] = {
    (either1, either2) match {
      case (_,        Left(e2)) => Left(e2)
      case (Right(f), Right(b)) => Right(f(b))
    }
  }
}

/** Import these implicits into a scope to treat Eithers as validations */
object EitherValidationImplicits {
  implicit def eitherWithLeftNothing2eitherWithLeftNothingValidation[E, X, Y](e: Either[Nothing, X => Y]) = EitherWithLeftNothingValidation(e)
  implicit def either2eitherValidation[E, T[E] <: TraversableLike[E, T[E]], X, Y](e: Either[T[E], X => Y]) = EitherValidation(e)
}
