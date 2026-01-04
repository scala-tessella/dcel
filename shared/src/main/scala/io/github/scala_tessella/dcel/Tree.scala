package io.github.scala_tessella.dcel

import scala.util.control.TailCalls.{TailRec, done, tailcall}

enum Tree[A]:
  case Leaf(_value: A)
  case Branch(_value: A, _children: List[Tree[A]])

  /** Destructor method. */
  def value: A =
    this match
      case Leaf(value)      => value
      case Branch(value, _) => value

  /** Destructor method. */
  def isLeaf: Boolean =
    this match
      case Leaf(_) => true
      case _       => false

  /** Destructor method. */
  def children: List[Tree[A]] =
    this match
      case Branch(_, children) => children
      case _                   => Nil

  /** Function from the algebraic data type and additional parameters to some generic type, as an abstraction
    * over structural recursion.
    *
    * @param fLeaf
    *   function converting the leaf value
    * @param fBranch
    *   function converting the branch value
    * @param gBranch
    *   function recursively converting the branch children
    * @tparam B
    *   The generic type of the value returned
    */
  def fold[B](fLeaf: A => B, fBranch: A => B, gBranch: List[B] => B): B =
    this match
      case Leaf(value)             => fLeaf(value)
      case Branch(value, children) =>
        gBranch(
          fBranch(value) :: children.map: child =>
            child.fold(fLeaf, fBranch, gBranch)
        )

  /** Same as fold but with the branch case in a single function; more ergonomic in some occasions.
    *
    * @param leaf
    *   function converting the leaf case (value only)
    * @param branch
    *   function converting the branch case (value and children)
    * @tparam B
    *   The generic type of the value returned
    */
  def foldAlt[B](leaf: A => B, branch: (A, List[B]) => B): B =
    this match
      case Leaf(value)             => leaf(value)
      case Branch(value, children) =>
        branch(
          value,
          children.map: child =>
            child.foldAlt(leaf, branch)
        )

  /** Transforms the children of a tree using a specified function and applies the transformation recursively.
    *
    * @param f
    *   A transformation function that takes a list of child trees and returns a transformed list of child
    *   trees.
    *
    * @return
    *   A new tree with its children transformed according to the provided function, applying the
    *   transformation recursively.
    */
  def transformChildren(f: List[Tree[A]] => List[Tree[A]]): Tree[A] =
    this match
      case Leaf(_)                 => this
      case Branch(value, children) =>
        val transformed = f(children).map: child =>
          child.transformChildren(f)
        Branch(value, transformed)

  /** Same as fold but tail recursive.
    * @see
    *   https://stackoverflow.com/questions/55042834/how-to-make-tree-mapping-tail-recursive
    */
  def tailRecFold[B](fLeaf: A => B, fBranch: A => B, gBranch: List[B] => B): B =

    // two inner functions doing mutual recursion

    // iterates recursively over children of node
    def iterate(nodes: List[Tree[A]]): TailRec[List[B]] =
      Tree.iterateRaw(nodes, deepMap)
//      nodes match
//        case Nil => done(Nil)
//        case h :: t =>
//          tailcall(deepMap(h)) //it calls with mutual recursion deepMap which maps over children of node
//            .flatMap { node => iterate(t).map { node :: _ } } //you can flat map over TailRec

    // recursively visits all branches
    def deepMap(node: Tree[A]): TailRec[B] =
      node match
        case Leaf(value)             =>
          done:
            fLeaf(value)
        case Branch(value, children) =>
          tailcall:
            iterate(children)
          .map: iteratedChildren =>
            gBranch(fBranch(value) :: iteratedChildren)

    deepMap(this).result // unwrap result to plain node

  /** Same as foldAlt but tail recursive. */
  def tailRecFoldAlt[B](leaf: A => B, branch: (A, List[B]) => B): B =

    def iterate(nodes: List[Tree[A]]): TailRec[List[B]] =
      Tree.iterateRaw(nodes, deepMap)

    def deepMap(node: Tree[A]): TailRec[B] =
      node match
        case Leaf(value)             =>
          done:
            leaf(value)
        case Branch(value, children) =>
          tailcall:
            iterate(children)
          .map: iteratedChildren =>
            branch(value, iteratedChildren)

    deepMap(this).result

  /** Simplified for when leaf and branch values can be treated in the same way. */
  def simpleFold[B](f: A => B, gBranch: List[B] => B): B =
    fold(f, f, gBranch)

  /** Returns the total number of nodes in the tree. */
  def size: Int =
//    this match
//      case Leaf(_)             => 1
//      case Branch(_, children) => children.map(_.size).sum + 1
    simpleFold(_ => 1, _.sum)

  /** Returns the number of leaf nodes in the tree. */
  def sizeLeaves: Int =
    fold(_ => 1, _ => 0, _.sum)

  /** Returns the depth of the tree. */
  def depth: Int =
    simpleFold(_ => 1, childrenDepths => 1 + (0 :: childrenDepths).max)

  /** Checks if the tree contains the specified element. */
  def contains(element: A): Boolean =
//    this match
//      case Leaf(value)             => value == element
//      case Branch(value, children) => value == element || children.exists(_.contains(element))
    simpleFold(
      _ == element,
      _.exists:
        identity
    )

  /** Maps leaf and branch values using separate functions.
    *
    * @param fLeaf
    *   Function to apply to leaf values
    * @param fBranch
    *   Function to apply to branch values
    * @return
    *   A new tree with transformed values
    */
  def mapLeavesAndBranches[B](fLeaf: A => B, fBranch: A => B): Tree[B] =
//    fold(
//      value => Leaf(fLeaf(value)),
//      value => Branch(fBranch(value), Nil),
//      nodes => Branch(nodes.head.value, nodes.tail)
//    )
    foldAlt(
      value => Leaf(fLeaf(value)),
      (value, children) => Branch(fBranch(value), children)
    )

  /** Maps all values in the tree using the provided function.
    *
    * @param f
    *   Function to apply to all values
    * @return
    *   A new tree with transformed values
    */
  def map[B](f: A => B): Tree[B] =
//    this match
//      case Leaf(value)             => Leaf(f(value))
//      case Branch(value, children) => Branch(f(value), children.map(_.map(f)))
    mapLeavesAndBranches(f, f)

  /** Maps only leaf values, leaving branch values unchanged.
    *
    * @param leaf
    *   Function to apply to leaf values
    * @return
    *   A new tree with transformed leaf values
    */
  def mapLeaves(leaf: A => A): Tree[A] =
    mapLeavesAndBranches(leaf, identity)

  /** Flattens the tree into a list of all values.
    *
    * @return
    *   A list containing all values in the tree
    */
  def flatten: List[A] =
    simpleFold(List(_), _.flatten)

  /** Flattens the tree into a list of leaf values only.
    *
    * @return
    *   A list containing only the leaf values
    */
  def flattenLeaves: List[A] =
    fold(List(_), _ => Nil, _.flatten)

  /** Just folds one level, cannot be defined in terms of fold. */
  def shrink(fLeaves: List[A] => A): Tree[A] =
    this match
      case Leaf(_)             => this
      case Branch(_, children) =>
        val shrunk: A =
          fLeaves(children.map: child =>
            child.value)
        if children.forall: child =>
            child.isLeaf
        then
          Leaf(shrunk)
        else
          Branch(
            shrunk,
            children.map: child =>
              child.shrink(fLeaves)
          )

  /** Substitutes branch values with op on leaves. */
  def shrinkAll(fLeaves: List[A] => A): Tree[A] =
    foldAlt(
      value => Leaf(value),
      (_, children) =>
        val shrunk: A =
          fLeaves(
            children.map: child =>
              child.value
          )
        if children.forall: child =>
            child.isLeaf
        then
          Leaf(shrunk)
        else
          Branch(shrunk, children)
    )

  /** Produces a simplified version of the tree by applying a binary folding operation on the values of branch
    * nodes and their children.
    *
    * @param fold
    *   A binary function that combines two values of type A into one. Applied to compress branch values and
    *   their child values.
    * @return
    *   A new tree with the branch values simplified according to the provided folding function, while
    *   maintaining the tree structure where necessary.
    */
  def compress(fold: (A, A) => A): Tree[A] =
    foldAlt(
      value => Leaf(value),
      (value, children) =>
        children match
          case Nil                 => Leaf(value)
          case Leaf(v) :: Nil      => Leaf(fold(value, v))
          case Branch(v, c) :: Nil => Branch(fold(value, v), c)
          case _                   => Branch(value, children)
    )

  def foldValues(gBranch: List[A] => A): A =
    simpleFold(identity, gBranch)

  /** Quicker traversing last child only, but not tail recursive. */
  def addNodeToLastChild(f: Tree[A] => Tree[A], acceptsNewChild: Tree[A] => Boolean): Tree[A] =

    def loop(node: Tree[A]): Tree[A] =
      node match
        case Leaf(_)                 => node
        case Branch(value, children) =>
          Branch(
            value,
            if acceptsNewChild(node) then children :+ f(node)
            else children.init :+ loop(children.last)
          )

    loop(this)

  def removeLastChildLeaf(f: Tree[A] => Tree[A]): Tree[A] =

    def loop(node: Tree[A]): Tree[A] =
      node match
        case Leaf(_)                 => node
        case Branch(value, children) =>
          Branch(
            value,
            if children.last.isLeaf then children.init
            else children.init :+ loop(f(children.last))
          )

    loop(this)

  def pruneLastEmptyBranch: Tree[A] =

    def loop(node: Tree[A]): Tree[A] =
      node match
        case Leaf(_)                 => node
        case Branch(value, children) =>
          if children.isEmpty || children.exists: child =>
              child.isLeaf
          then
            node
          else
            Branch(
              value,
              if children.last.children.isEmpty then children.init
              else children.init :+ loop(children.last)
            )

    loop(this)

  /** Adds one leaf to an existing branch node
    *
    * @param elem
    *   the added leaf's value
    * @param acceptsNewChild
    *   checks if the branch supports the addition
    * @todo
    *   test how it works if the condition is never satisfied
    */
  def addLeaf(elem: A, acceptsNewChild: Tree[A] => Boolean): Tree[A] =
    addNodeToLastChild(_ => Leaf(elem), acceptsNewChild)

  /** Export to a DOT graph */
  def toDOT(labeler: A => String = (_: A) => ""): String =
    var ordinal: Int           =
      1
    var elements: List[String] =
      Nil

    def loop(assignedOrdinal: Int, tree: Tree[A]): Unit =
      val label: String =
        labeler(tree.value) match
          case e if e.isEmpty => ""
          case additional     => s""" [label="$additional"]"""
      elements ::= assignedOrdinal.toString + label
      tree match
        case Leaf(_)             => ()
        case Branch(_, children) =>
          children.foreach: child =>
            ordinal += 1
            elements ::= s"$assignedOrdinal -- $ordinal"
            loop(ordinal, child)

    loop(ordinal, this)
    ("graph G {" :: ("}" :: elements).reverse).mkString("\n")

  def ensureDepthOneBranchesHaveValidValues(isInvalid: A => Boolean, fromChildren: List[Tree[A]] => A): Tree[A] =
    this match
      case Leaf(value) => Leaf(value)
      case Branch(value, children) =>
        val newChildren =
          children
            .map:
              case Branch(value, children) if isInvalid(value) => Branch(fromChildren(children), children)
              case child => child
        Branch(value, newChildren)

object Tree:

  /** Iterate step for tail recursion, iterates recursively over children of node.
    *
    * @param nodes
    *   children of node
    * @param deepMap
    *   function on child
    * @tparam T
    * @tparam U
    */
  private def iterateRaw[T, U](nodes: List[T], deepMap: T => TailRec[U]): TailRec[List[U]] =
    nodes match
      case Nil    => done(Nil)
      case h :: t =>
        // calls with mutual recursion deepMap which maps over children of node
        tailcall:
          deepMap(h)
        .flatMap: node => // you can flat map over TailRec
          iterateRaw(t, deepMap).map: nodes =>
            node :: nodes

  /** Builds up a tree from a seed value using co-recursion. This is the dual of fold - an abstraction over
    * structural co-recursion.
    *
    * @param seed
    *   Initial value to start building the tree
    * @param stop
    *   Predicate that determines when to stop recursion and create a leaf
    * @param fLeaf
    *   Function to transform seed value into leaf value
    * @param fBranch
    *   Function to transform seed value into branch value
    * @param next
    *   Function to generate child seed values from the current seed
    * @tparam A
    *   Type of the seed value
    * @tparam B
    *   Type of the values in the resulting tree
    * @return
    *   A tree built from the seed value
    */
  def unfold[A, B](seed: A)(stop: A => Boolean, fLeaf: A => B, fBranch: A => B, next: A => List[A]): Tree[B] =
    if stop(seed) then Leaf(fLeaf(seed))
    else
      Branch(
        fBranch(seed),
        next(seed).map:
          unfold(_)(stop, fLeaf, fBranch, next)
      )

  /** Same as unfold but with tail recursion for better performance with large trees. */
  def tailRecUnfold[A, B](seed: A)(
      stop: A => Boolean,
      fLeaf: A => B,
      fBranch: A => B,
      next: A => List[A]
  ): Tree[B] =

    def iterate(nodes: List[A]): TailRec[List[Tree[B]]] =
      iterateRaw(nodes, deepMap)

    def deepMap(initial: A): TailRec[Tree[B]] =
      if stop(initial) then
        done:
          Leaf(fLeaf(initial))
      else
        tailcall:
          iterate(next(initial))
        .map: values =>
          Branch(fBranch(initial), values)

    deepMap(seed).result

  /** Creates a tree with a specified depth, filling all nodes with the same value.
    *
    * @param n
    *   The depth of the tree
    * @param elem
    *   The value to fill the tree with (lazily evaluated)
    * @tparam A
    *   The type of the values in the tree
    * @return
    *   A tree filled with the specified value
    */
  def fill[A](n: Int)(elem: => A): Tree[A] =
    unfold(n)(_ == 0, _ => elem, _ => elem, x => List.fill(n)(x - 1))

  /** Maps all values in the given tree using the provided function. This is an alternative to the instance
    * method map.
    *
    * @param node2
    *   The tree to map
    * @param f
    *   The function to apply to each value
    * @tparam A
    *   The type of the values in the original tree
    * @tparam B
    *   The type of the values in the resulting tree
    * @return
    *   A new tree with transformed values
    */
  def map[A, B](node2: Tree[A], f: A => B): Tree[B] =
    unfold(node2)(_.isLeaf, leaf => f(leaf.value), branch => f(branch.value), _.children)
