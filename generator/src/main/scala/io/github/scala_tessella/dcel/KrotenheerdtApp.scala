package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.TilingCertifier.Certified
import io.github.scala_tessella.dcel.conversion.TilingSVG.{toMetadataXml, toScalableVectorGraphics}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/** Driver for the Krotenheerdt enumeration (OEIS A068600, ADR-0018). Runs [[KrotenheerdtSearch.enumerate]]
  * and persists every certified tiling as it is found, so a long run leaves usable artifacts even if
  * interrupted:
  *
  *   - `tiling-<key>.svg` — a render for eyeball comparison against Galebach's gallery;
  *   - `tiling-<key>.xml` — the patch metadata (round-trips via `TilingSVG.fromMetadata`);
  *   - `index.tsv` — one line per distinct tiling: composition, torus key, basis, certifying patch size.
  *
  * Usage: `runMain io.github.scala_tessella.dcel.KrotenheerdtApp <n> <maxVertices> [outDir] [parallelism]
  * [hardCapFactor] [earlyTypeGate] [typeBallRadius]`.
  *
  * The published targets are A068600: 11, 20, 39, 33, 15, 10, 7 for n = 1..7, then 0.
  */
object KrotenheerdtApp:

  def main(args: Array[String]): Unit =
    val n              = args(0).toInt
    val maxVertices    = args(1).toInt
    val outDir         = Paths.get(args.lift(2).getOrElse(s"generator/results/n$n"))
    val parallelism    = args.lift(3).map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors)
    val hardCapFactor  = args.lift(4).map(_.toDouble).getOrElse(4.5)
    val earlyTypeGate  = args.lift(5).map(_.toInt).getOrElse(60)
    val typeBallRadius = args.lift(6).map(_.toInt).getOrElse(3)

    Files.createDirectories(outDir)
    val index = outDir.resolve("index.tsv")
    Files.writeString(index, "composition\ttorusKey\tbasis\tpatchVertices\n")

    val started = System.nanoTime
    println(
      s"n=$n maxVertices=$maxVertices parallelism=$parallelism -> $outDir " +
        s"(A068600 target: ${published.lift(n - 1).map(_.toString).getOrElse("?")})"
    )

    val lock                        = new Object
    def persist(c: Certified): Unit = lock.synchronized {
      val composition = c.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; ")
      val slug        = f"${math.abs(c.torusKey.hashCode)}%08x"
      write(outDir.resolve(s"tiling-$slug.svg"), c.patch.toScalableVectorGraphics())
      write(outDir.resolve(s"tiling-$slug.xml"), c.patch.toMetadataXml)
      val basis       =
        s"(${c.basis._1.x.toDouble},${c.basis._1.y.toDouble})/(${c.basis._2.x.toDouble},${c.basis._2.y.toDouble})"
      Files.writeString(
        index,
        s"$composition\t${c.torusKey}\t$basis\t${c.patch.vertices.size}\n",
        StandardOpenOption.APPEND
      ): Unit
    }

    val outcome = KrotenheerdtSearch.enumerate(
      n,
      maxVertices,
      hardCapFactor = hardCapFactor,
      earlyTypeGate = earlyTypeGate,
      typeBallRadius = typeBallRadius,
      parallelism = parallelism,
      onFound = persist,
      log = msg => { println(msg); System.out.flush() }
    )

    val secs          = (System.nanoTime - started) / 1e9
    val byComposition =
      outcome.certified
        .groupBy(_.vertexTypes.map(_.mkString(".")).toList.sorted.mkString("; "))
        .view.mapValues(_.size).toMap

    println(
      f"%n=== n=$n: ${outcome.certified.size} tilings in ${secs}%.0f s, ${outcome.statesExplored} states ==="
    )
    byComposition.toList.sortBy(_._1).foreach((comp, count) => println(s"  ${count}x  $comp"))
    println(s"rejections: ${outcome.rejections}")
    published.lift(n - 1).foreach(target =>
      println(if outcome.certified.size == target then s"MATCHES A068600($n) = $target"
      else s"MISMATCH: found ${outcome.certified.size}, A068600($n) = $target")
    )

  /** OEIS A068600, n = 1..8. */
  private val published = List(11, 20, 39, 33, 15, 10, 7, 0)

  private def write(path: Path, content: String): Unit =
    Files.writeString(path, content): Unit
