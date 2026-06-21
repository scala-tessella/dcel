package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.KrotenheerdtTorusMapSearch.{FaceZ, completeVertexTypes}

/** Phase-2 DIAGNOSTIC: trace the m=6 symmetric grow from the central hexagon to see WHY only 6.6.6 closes.
  * For every search state, records (face-count, did-it-close, the distinct complete-vertex types present and
  * the distinct face-size MULTISET), then prints aggregates: how many states reach each face-count, which
  * complete-vertex types ever form, and the type-sets of states that closed.
  *
  * Run: `generator/Test/runMain io.github.scala_tessella.dcel.SymmetryTraceProbe [maxFaces]`
  */
object SymmetryTraceProbe:

  def main(args: Array[String]): Unit =
    val maxFaces = args.headOption.map(_.toInt).getOrElse(20)

    val byFaceCount = scala.collection.mutable.Map.empty[Int, Int]
    val typesSeen   = scala.collection.mutable.Set.empty[String]
    val closedTypes = scala.collection.mutable.Set.empty[String]
    var maxReached  = 0
    var closedCount = 0

    val res = KrotenheerdtTorusMapSearch.enumerateBySymmetry(
      m = 6,
      maxN = 1,
      maxFaces = maxFaces,
      onState = (faces, closed) =>
        byFaceCount(faces.size) = byFaceCount.getOrElse(faces.size, 0) + 1
        maxReached = math.max(maxReached, faces.size)
        completeVertexTypes(faces).foreach(t => typesSeen += t.mkString("."))
        if closed then
          closedCount += 1
          completeVertexTypes(faces).foreach(t => closedTypes += t.mkString("."))
          // print the face-size multiset of the FIRST few closed patches
          if closedCount <= 8 then
            val sizes = faces.map(_.size).groupBy(identity).view.mapValues(_.size).toMap
            println(
              s"  CLOSED at ${faces.size} faces: sizes=$sizes  completeTypes=${completeVertexTypes(faces).map(_.mkString("."))}"
            )
    )

    println(
      s"\nstates=${res.states}, maxFacesReached=$maxReached, budgetHit=${res.budgetHit}, closedStates=$closedCount"
    )
    println(s"tilings found: ${res.tilings.map(_._2.map(_.mkString(".")))}")
    println(s"distinct COMPLETE vertex types ever formed: ${typesSeen.toList.sorted}")
    println(s"complete types in CLOSED patches: ${closedTypes.toList.sorted}")
    println("states by face-count: " + byFaceCount.toList.sortBy(_._1).map((f, c) => s"$f→$c").mkString(" "))
