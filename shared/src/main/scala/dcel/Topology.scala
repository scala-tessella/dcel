package dcel

import scala.collection.mutable

object Topology:

  def breadthFirstSearch[T](start: T, adjacency: Map[T, List[T]]): Set[T] =
    val visited = mutable.Set[T](start)
    val queue   = mutable.Queue[T](start)

    while queue.nonEmpty do
      val current = queue.dequeue()
      adjacency.getOrElse(current, Nil).foreach { neighbor =>

        if !visited.contains(neighbor) then
          visited += neighbor
          queue.enqueue(neighbor)
      }

    visited.toSet
