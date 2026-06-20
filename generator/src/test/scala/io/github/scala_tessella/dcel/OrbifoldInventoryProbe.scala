package io.github.scala_tessella.dcel

/** ADR-0023 euclidean-orbifold generator — STAGE 0 (de-risk): characterize the orbifolds that the n≤3 minimal
  * symbols actually live on. The orbifold-directed plan is "fix a euclidean orbifold, enumerate its
  * triangulations"; this prints, for the validated oracle output, HOW MANY distinct orbifolds appear, how
  * small they are, and their cone/mirror structure — the data that says whether fix-and-enumerate is
  * tractable and how to build it.
  */
object OrbifoldInventoryProbe:

  def main(args: Array[String]): Unit =
    val maxN    = args.headOption.map(_.toInt).getOrElse(3)
    val maxSize = args.lift(1).map(_.toInt).getOrElse(24)

    // How much of the generate-all tree is the hyperbolic universe an orbifold-directed generator would skip?
    println("generate-all waste (total D-sets : euclidean-feasible : regular-euclidean symbols):")
    for sz <- List(10, 12, 14, 16, 18) do
      val (tot, eu, reg) = DelaneySymbols.generationStats(maxN, sz)
      println(
        f"  maxSize=$sz%2d  total=$tot%9d  euclidean=$eu%7d (${100.0 * eu / tot}%5.2f%%)  regular=$reg%4d"
      )

    println(s"enumerateSymbols(maxN=$maxN, maxSize=$maxSize) ...")
    val t0   = System.nanoTime()
    val syms = DelaneySymbols.enumerateSymbols(maxN, maxSize)
    val ms   = (System.nanoTime() - t0) / 1000000
    println(s"got ${syms.size} tilings in ${ms}ms")

    // per-n size range
    for n <- 1 to maxN do
      val sizes = syms.filter(_._1 == n).map(_._3.size)
      if sizes.nonEmpty then
        println(f"  n=$n  tilings=${sizes.size}%2d  chambers ${sizes.min}..${sizes.max}")

    // distinct orbifold signatures and how many tilings sit on each
    val byOrb =
      syms.groupBy(t => DelaneySymbols.orbifoldSignature(t._3)).view.mapValues(_.size).toList.sortBy(-_._2)
    println(s"distinct orbifold signatures: ${byOrb.size}")
    for (sig, count) <- byOrb do println(f"  ${count}%3d  $sig")
