package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}

/** Maps the minimal torus cell size V and assembly cost for every 2-uniform bucket — the decisive data for
  * whether the bounded-V assembler is COMPLETE and how cost scales with cell size (the large-cell wall).
  */
object BucketCellProbe:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  // distinct 2-uniform type-sets with their A068600 multiplicity (how many distinct tilings share the set)
  private val n2 = List(
    Set("3.3.3.3.3.3", "3.3.4.3.4") -> 1,
    Set("3.4.6.4", "3.3.4.3.4")     -> 1,
    Set("3.4.6.4", "3.3.3.4.4")     -> 1,
    Set("3.4.6.4", "3.4.4.6")       -> 1,
    Set("4.6.12", "3.4.6.4")        -> 1,
    Set("3.3.3.3.3.3", "3.3.4.12")  -> 1,
    Set("3.12.12", "3.4.3.12")      -> 1,
    Set("3.3.3.3.3.3", "3.3.6.6")   -> 1,
    Set("3.3.3.3.3.3", "3.3.3.3.6") -> 2,
    Set("3.3.6.6", "3.3.3.3.6")     -> 1,
    Set("3.6.3.6", "3.3.6.6")       -> 1,
    Set("3.4.4.6", "3.6.3.6")       -> 2,
    Set("3.3.3.4.4", "3.3.4.3.4")   -> 2,
    Set("4.4.4.4", "3.3.3.4.4")     -> 2,
    Set("3.3.3.3.3.3", "3.3.3.4.4") -> 2
  )

  def main(args: Array[String]): Unit =
    val maxV       = args.headOption.map(_.toInt).getOrElse(6)
    val budget     = 3_000_000L
    println(s"bucket minimal-V scan (maxV=$maxV, budget=$budget)")
    var foundCount = 0
    var expected   = 0
    for (types, mult) <- n2 do
      val bucket = types.map(sig)
      expected += mult
      val r      = BucketAssembly.enumerateBucket(bucket, maxV, budget)
      foundCount += r.tilings.size
      val label  =
        types.map(_.replace("3.3.3.3.3.3", "3^6").replace("3.3.3.3.6", "3^4.6")).mkString("{", ";", "}")
      val flag   = if r.tilings.size == mult then "ok " else if r.tilings.size < mult then "LOW" else "HI!"
      println(
        f"$flag $label%-34s found=${r.tilings.size}/$mult  states=${r.states}%9d  closed=${r.mapsClosed}%6d  budgetHit=${r.budgetHit}"
      )
    println(s"TOTAL found=$foundCount expected(<=maxV)=$expected of 20")
