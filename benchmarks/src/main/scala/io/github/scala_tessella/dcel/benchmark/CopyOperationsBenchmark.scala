package io.github.scala_tessella.dcel.benchmark

import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.geometry.BigPoint

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

/** Scaling behaviour of the isometric-copy operations, which all funnel through [[TilingMerge.mergeTilings]]
  * plus a full [[TilingValidation.validate]] after every merge. The merge has known O(n²) spots (coincidence
  * matching, boundary ordering), so this benchmark watches how `fanAround`, `maybeAddTranslatedCopy` and
  * `repeatGrid` grow with an `size × size` square grid — the kind of large pattern the editor would build
  * from these operations.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch", "-XX:+UseParallelGC"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class CopyOperationsBenchmark:

  @Param(Array("4", "8", "12", "16"))
  var size: Int = uninitialized

  private var base: TilingDCEL   = uninitialized
  private var centre: BigPoint   = uninitialized
  private var stepFrom: BigPoint = uninitialized
  private var stepTo: BigPoint   = uninitialized
  private var rightTo: BigPoint  = uninitialized
  private var upTo: BigPoint     = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    base = CopyOperationsBenchmark.unsafe(TilingBuilder.createRhombusNet(size, size), s"net $size")
    centre = base.coordinates.values.toList.centroid
    // One-cell translation vector (overlaps all but one column -> heavy coincidence matching).
    stepFrom = BigPoint.origin
    stepTo = BigPoint(BigDecimal(1), BigDecimal(0))
    // Full-width / full-height steps for a 2x2 lattice of disjoint copies.
    rightTo = BigPoint(BigDecimal(size), BigDecimal(0))
    upTo = BigPoint(BigDecimal(0), BigDecimal(size))

  /** Full 4-fold ring about the grid's centre: 3 merges, every copy a full overlap that must deduplicate. */
  @Benchmark
  def fanAroundOrder4(bh: Blackhole): Unit =
    bh.consume(base.fanAround(centre, 4))

  /** A single overlapping translate-and-merge: isolates the merge + validate cost on two grid-sized inputs.
    */
  @Benchmark
  def translatedCopyOverlapping(bh: Blackhole): Unit =
    bh.consume(base.maybeAddTranslatedCopy(stepFrom, stepTo))

  /** A 2x2 lattice of the grid (3 disjoint merges): the boundary-ordering / rebuild cost without overlap. */
  @Benchmark
  def repeatGrid2x2(bh: Blackhole): Unit =
    bh.consume(base.repeatGrid(BigPoint.origin, rightTo, 2, upTo, 2))

object CopyOperationsBenchmark:

  private def unsafe[A](either: Either[TilingError, A], label: String): A =
    either match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalArgumentException(s"Benchmark fixture '$label' failed: ${error.message}")
