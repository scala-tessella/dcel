package io.github.scala_tessella.dcel

object Utils:

  extension [E, A](eithers: List[Either[E, A]])

    /** Transforms a list of `Either` values into a single `Either` containing a list of successful values or
      * the first encountered error.
      *
      * @return
      *   an `Either` containing:
      *   - `Right(List[A])` if all elements in the list are `Right`
      *   - `Left(E)` if at least one element in the list is `Left`, with the first error encountered.
      */
    def sequence: Either[E, List[A]] =
      eithers.foldRight(Right(Nil): Either[E, List[A]]): (e, acc) =>
        for
          xs <- acc
          x  <- e
        yield x :: xs

  extension [A](maybeA: Option[A])

    /** Applies a function to the value inside an Option, propagating errors using Either.
      *
      * @param f
      *   the function to be applied to the value inside the Option, producing an Either result.
      * @return
      *   either an error of type E or an Option containing the transformed value of type B.
      */
    def traverse[E, B](f: A => Either[E, B]): Either[E, Option[B]] =
      maybeA match
        case Some(a) =>
          f(a).map: b =>
            Some(b)
        case None    => Right(None)

  extension [A](seq: Seq[A])

    /** Convert to a `Map` where key is the element and value is a function applied to it
      *
      * @param f
      *   the function transforming each element
      * @return
      *   a `Map` mapping each element to its transformation.
      * @example
      *   {{{List(1, 2).associate(_ + 1) // Map(1 -> 2, 2 -> 3)}}}
      */
    def associate[T](f: A => T): Map[A, T] =
      seq.iterator
        .map: elem =>
          elem -> f(elem)
        .toMap

    /** Creates a `Map` where each key is the result of applying a function to the elements of the sequence
      * and the corresponding value is the original element.
      *
      * @param f
      *   the function used to transform each element to a key in the resulting `Map`.
      * @return
      *   a `Map` where keys are the transformed elements and values are the original elements.
      */
    def associateValues[T](f: A => T): Map[T, A] =
      seq.iterator
        .map: elem =>
          f(elem) -> elem
        .toMap
