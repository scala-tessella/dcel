package io.github.scala_tessella.dcel.benchmark

import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.TilingUniformity.{scanUniformityTree, uniformityTreeUncompressed}

import scala.util.Try

object UniformityBenchmark:

  @volatile private var sink: Long = 0L

  final private case class Config(
      warmupIterations: Int = 3,
      measuredIterations: Int = 8
  )

  final private case class BenchCase(name: String, tiling: TilingDCEL)

  final private case class Op(name: String, run: TilingDCEL => Long)

  private def unsafe[A](either: Either[TilingError, A], label: String): A =
    either match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalArgumentException(s"Could not build benchmark case '$label': ${error.message}")

  private def parseConfig(args: Array[String]): Config =
    args.foldLeft(Config()): (config, arg) =>
      if arg.startsWith("--warmup=") then
        val value = arg.stripPrefix("--warmup=")
        Try(value.toInt).toOption.filter(_ >= 0).map(v => config.copy(warmupIterations = v)).getOrElse(config)
      else if arg.startsWith("--runs=") then
        val value = arg.stripPrefix("--runs=")
        Try(
          value.toInt
        ).toOption.filter(_ > 0).map(v => config.copy(measuredIterations = v)).getOrElse(config)
      else
        config

  private def benchmarkCases: List[BenchCase] =
    List(
      BenchCase("triangle-net-8x8", unsafe(TilingBuilder.createTriangleNet(8, 8), "triangle-net-8x8")),
      BenchCase("rhombus-net-8x8", unsafe(TilingBuilder.createRhombusNet(8, 8), "rhombus-net-8x8")),
      BenchCase("hexagon-net-5x5", unsafe(TilingBuilder.createHexagonNet(5, 5), "hexagon-net-5x5")),
      BenchCase(
        "holed-triangle-10x10",
        unsafe(
          TilingBuilder.createHoledTriangleNet(10, 10): (x, y) =>
            x > 2 && x < 7 && y > 2 && y < 7,
          "holed-triangle-10x10"
        )
      ),
      BenchCase("ring-12", unsafe(TilingBuilder.createRing(RegularPolygon(12)), "ring-12"))
    )

  private def operations: List[Op] =
    List(
      Op(
        "uniformityTreeUncompressed",
        { tiling =>
          val tree = tiling.uniformityTreeUncompressed(None)
          tree.size.toLong + tree.sizeLeaves.toLong
        }
      ),
      Op(
        "uniformityTree",
        { tiling =>
          val tree = tiling.uniformityTree
          tree.size.toLong + tree.sizeLeaves.toLong
        }
      ),
      Op(
        "scanUniformityTree",
        { tiling =>
          val trees = tiling.scanUniformityTree
          trees.map(_.sizeLeaves.toLong).sum + trees.length
        }
      )
    )

  private def percentile(sorted: Vector[Long], p: Double): Long =
    if sorted.isEmpty then 0L
    else
      val index = math.min(sorted.length - 1, math.max(0, math.round((sorted.length - 1) * p).toInt))
      sorted(index)

  private def formatMs(nanos: Long): String =
    f"${nanos.toDouble / 1e6}%.3f"

  private def runOp(config: Config, op: Op, tiling: TilingDCEL): Vector[Long] =
    var i = 0
    while i < config.warmupIterations do
      sink ^= op.run(tiling)
      i += 1

    Vector.tabulate(config.measuredIterations): _ =>
      val t0 = System.nanoTime()
      sink ^= op.run(tiling)
      val t1 = System.nanoTime()
      t1 - t0

  def main(args: Array[String]): Unit =
    val config = parseConfig(args)
    println(
      s"Uniformity benchmark (warmup=${config.warmupIterations}, runs=${config.measuredIterations})"
    )
    println("case,operation,min_ms,median_ms,p95_ms,mean_ms")

    benchmarkCases.foreach: benchCase =>
      operations.foreach: op =>
        val samples = runOp(config, op, benchCase.tiling)
        val sorted  = samples.sorted
        val minN    = sorted.headOption.getOrElse(0L)
        val medN    = percentile(sorted, 0.5)
        val p95N    = percentile(sorted, 0.95)
        val meanN   = if samples.nonEmpty then samples.sum / samples.length else 0L

        println(
          s"${benchCase.name},${op.name},${formatMs(minN)},${formatMs(medN)},${formatMs(p95N)},${formatMs(meanN)}"
        )

    println(s"blackhole=$sink")
