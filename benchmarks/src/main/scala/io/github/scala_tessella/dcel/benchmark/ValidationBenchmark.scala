package io.github.scala_tessella.dcel.benchmark

import io.github.scala_tessella.dcel.*
import io.github.scala_tessella.dcel.TilingValidation.validate
import io.github.scala_tessella.dcel.conversion.TilingSVG.fromMetadata

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms2g", "-Xmx2g", "-XX:+AlwaysPreTouch", "-XX:+UseParallelGC"))
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
class ValidationBenchmark:

  @Param(
    Array(
      "aperiodic/domino",
      "regular/regular_6-6-6",
      "semiregular/semiregular_3-6-3-6",
      "semiregular/semiregular_3-3-4-3-4",
      "semiregular/semiregular_4-6-12"
    )
  )
  var fixture: String = uninitialized

  private var metadata: String   = uninitialized
  private var tiling: TilingDCEL = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    metadata = ValidationBenchmark.loadFixture(fixture)
    tiling = ValidationBenchmark.unsafe(fromMetadata(metadata), fixture)

  /** End-to-end import: parse metadata + run the full validation pipeline. Mirrors the editor's
    * `SvgImporter.parseTiling` hot path.
    */
  @Benchmark
  def fromMetadataImport(bh: Blackhole): Unit =
    bh.consume(ValidationBenchmark.unsafe(fromMetadata(metadata), fixture))

  /** Validation only, on a pre-parsed tiling. Isolates the geometry / spatial checks that dominate
    * `fromMetadata` according to `dcel-validation-perf-investigation.md`.
    */
  @Benchmark
  def validateOnly(bh: Blackhole): Unit =
    bh.consume(validate(tiling))

object ValidationBenchmark:

  private val TemplatesSuffix: Path =
    Paths.get("shared", "src", "test", "resources", "templates")

  /** JMH forks run with a working directory that may be the subproject, not the repo root. Walk up from
    * `user.dir` until we find the shared templates tree.
    */
  private lazy val TemplatesRoot: Path =
    val start = Paths.get(sys.props("user.dir")).toAbsolutePath
    Iterator
      .iterate[Path](start)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(TemplatesSuffix))
      .find(Files.isDirectory(_))
      .getOrElse(
        throw new IllegalStateException(
          s"Could not locate $TemplatesSuffix starting from $start"
        )
      )

  private def unsafe[A](either: Either[TilingError, A], label: String): A =
    either match
      case Right(value) => value
      case Left(error)  =>
        throw new IllegalArgumentException(
          s"Benchmark fixture '$label' failed: ${error.message}"
        )

  private def loadFixture(name: String): String =
    val file = TemplatesRoot.resolve(s"$name.svg")
    if !Files.exists(file) then
      throw new IllegalArgumentException(
        s"Benchmark fixture not found: $file"
      )
    Files.readString(file)
