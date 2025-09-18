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
