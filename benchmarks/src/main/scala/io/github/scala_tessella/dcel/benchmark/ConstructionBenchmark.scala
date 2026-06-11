package io.github.scala_tessella.dcel.benchmark

import io.github.scala_tessella.dcel.*

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** End-to-end cost of building tilings polygon by polygon through the growth pipeline ([[TilingAddition]]
  * plus per-step validation) — the path the editor exercises on every edit, and one no other benchmark
  * measures (the other suites build their fixtures in setup, outside the timed region).
  *
  * Each addition walks the existing boundary, so the builders compound any super-linear cost hiding in a
  * single growth step; this is the benchmark that makes regressions in that pipeline visible as a
  * worse-than-expected growth curve over `size`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch", "-XX:+UseParallelGC"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class ConstructionBenchmark:

  @Param(Array("4", "8", "12", "16"))
  var size: Int = uninitialized

  @Benchmark
  def triangleNet(bh: Blackhole): Unit =
    bh.consume(ConstructionBenchmark.unsafe(TilingBuilder.createTriangleNet(size, size), s"triangle $size"))

  @Benchmark
  def rhombusNet(bh: Blackhole): Unit =
    bh.consume(ConstructionBenchmark.unsafe(TilingBuilder.createRhombusNet(size, size), s"rhombus $size"))

  @Benchmark
  def hexagonNet(bh: Blackhole): Unit =
    bh.consume(ConstructionBenchmark.unsafe(TilingBuilder.createHexagonNet(size, size), s"hexagon $size"))

object ConstructionBenchmark:

  private def unsafe[A](either: Either[TilingError, A], label: String): A =
    either match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalArgumentException(s"Benchmark fixture '$label' failed: ${error.message}")
