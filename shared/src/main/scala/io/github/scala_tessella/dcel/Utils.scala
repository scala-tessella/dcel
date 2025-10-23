package io.github.scala_tessella.dcel

object Utils:

  extension [E, A](eithers: List[Either[E, A]])
    def sequence: Either[E, List[A]] =
      eithers.foldRight(Right(Nil): Either[E, List[A]]) { (e, acc) =>

        for
          xs <- acc
          x  <- e
        yield x :: xs
      }

  extension [A](opt: Option[A])
    def traverse[E, B](f: A => Either[E, B]): Either[E, Option[B]] =
      opt match
        case Some(a) => f(a).map(Some(_))
        case None    => Right(None)

  extension [A](seq: Seq[A])

    /** Convert to a `Map` where key is the element and value is a function applied to it
     *
     * @param f the function transforming each element
     * @return a `Map` mapping each element to its transformation.
     * @example {{{List(1, 2).toMap2(_ + 1) // Map(1 -> 2, 2 -> 3)}}}
     */
    def toMap2[T](f: A => T): Map[A, T] =
      seq.view.map(elem => elem -> f(elem)).toMap