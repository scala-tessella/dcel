package io.github.scala_tessella.dcel.benchmark

import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.TilingUniformity.{scanUniformityTree, uniformityTreeUncompressed}
import io.github.scala_tessella.dcel.geometry.RegularPolygon

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch", "-XX:+UseParallelGC"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class UniformityBenchmark:

  @Param(
    Array(
      "triangle-net-8x8",
      "rhombus-net-8x8",
      "hexagon-net-5x5",
      "holed-triangle-10x10",
      "ring-12"
    )
  )
  var caseName: String = uninitialized

  private var tiling: TilingDCEL = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    tiling = UniformityBenchmark.buildCase(caseName)

  @Benchmark
  def uniformityTreeUncompressed(bh: Blackhole): Unit =
    bh.consume(tiling.uniformityTreeUncompressed(None))

  @Benchmark
  def uniformityTree(bh: Blackhole): Unit =
    bh.consume(tiling.uniformityTree)

  @Benchmark
  def scanUniformityTree(bh: Blackhole): Unit =
    bh.consume(tiling.scanUniformityTree)

object UniformityBenchmark:

  private def unsafe[A](either: Either[TilingError, A], label: String): A =
    either match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalArgumentException(
          s"Could not build benchmark case '$label': ${error.message}"
        )

  private def buildCase(name: String): TilingDCEL =
    name match
      case "triangle-net-8x8"     =>
        unsafe(TilingBuilder.createTriangleNet(8, 8), name)
      case "rhombus-net-8x8"      =>
        unsafe(TilingBuilder.createRhombusNet(8, 8), name)
      case "hexagon-net-5x5"      =>
        unsafe(TilingBuilder.createHexagonNet(5, 5), name)
      case "holed-triangle-10x10" =>
        unsafe(
          TilingBuilder.createHoledTriangleNet(10, 10): (x, y) =>
            x > 2 && x < 7 && y > 2 && y < 7,
          name
        )
      case "ring-12"              =>
        unsafe(TilingBuilder.createRing(RegularPolygon(12)), name)
      case other                  =>
        throw new IllegalArgumentException(s"Unknown benchmark case: $other")
