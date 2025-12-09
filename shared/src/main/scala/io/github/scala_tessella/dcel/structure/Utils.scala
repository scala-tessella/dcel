package io.github.scala_tessella.dcel.structure

import scala.collection.mutable

object Utils:

  def breadthFirstSearch[T](start: T, adjacency: Map[T, List[T]]): Set[T] =
    val visited = mutable.Set[T](start)
    val queue   = mutable.Queue[T](start)

    while queue.nonEmpty do
      val current = queue.dequeue()
      adjacency.getOrElse(current, Nil).foreach: neighbor =>
        if !visited.contains(neighbor) then
          visited += neighbor
          queue.enqueue(neighbor)

    visited.toSet

  /** Shortest path (unweighted) from `start` to `goal` in a graph defined by `adjacency`. Optionally excludes
    * any vertex present in `excluded` (neither visited nor returned).
    *
    * Returns:
    *   - Some(List(start, ..., goal)) if a path exists
    *   - None if no path exists
    */
  def shortestPath[T](
      start: T,
      goal: T,
      adjacency: Map[T, List[T]],
      excluded: Set[T] = Set.empty[T]
  ): List[T] =
    if excluded.contains(start) || excluded.contains(goal) then return Nil
    if start == goal then return List(start)

    val visited = mutable.Set[T](start)
    val queue   = mutable.Queue[T](start)
    val parent  = mutable.Map[T, T]() // child -> parent

    while queue.nonEmpty do
      val current = queue.dequeue()
      // If current is the goal, reconstruct the path immediately
      if current == goal then
        val pathBuf = mutable.ListBuffer[T](current)
        var node    = current
        while parent.contains(node) do
          val p = parent(node)
          pathBuf.prepend(p)
          node = p
        return pathBuf.toList

      val neighbors = adjacency.getOrElse(current, Nil)
      neighbors.foreach: neighbor =>
        if !excluded.contains(neighbor) && !visited.contains(neighbor) then
          visited += neighbor
          parent(neighbor) = current
          queue.enqueue(neighbor)

    Nil
