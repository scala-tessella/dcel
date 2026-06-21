package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.VertexTypes.{VertexSignature, normalize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Fast guards for the ADR-0030 multi-bucket completion driver ([[BucketAssembly.enumerateBuckets]]) and the
  * Wikipedia-config parser, so the driver mechanics (parallel fan-out + global dedup) are pinned before the
  * long n=4 run and before any keying optimisation inside the validated `Assembler`.
  */
class BucketDriverSpec extends AnyFlatSpec with Matchers:

  private def sig(s: String): VertexSignature = normalize(s.split('.').map(_.toInt).toList)

  behavior of "N4DriverProbe.parseConfig (Wikipedia compact notation)"

  it should "expand exponents and normalise" in:
    N4DriverProbe.parseConfig("3^6") shouldBe sig("3.3.3.3.3.3")
    N4DriverProbe.parseConfig("3^2.4.3.4") shouldBe sig("3.3.4.3.4")
    N4DriverProbe.parseConfig("3.12^2") shouldBe sig("3.12.12")
    N4DriverProbe.parseConfig("4^4") shouldBe sig("4.4.4.4")
    N4DriverProbe.parseConfig("3^3.4^2") shouldBe sig("3.3.3.4.4")

  it should "parse a full 4-type row into four distinct sigs" in:
    N4DriverProbe.parseRow("3^6; 3^4.6; 3^2.6^2; 6^3") shouldBe
      Set(sig("3.3.3.3.3.3"), sig("3.3.3.3.6"), sig("3.3.6.6"), sig("6.6.6"))

  behavior of "BucketAssembly.enumerateBuckets (driver fan-out + global dedup)"

  // the three regular (k=1) tilings: small cells (V<=3), so the driver runs in well under a second
  private val k1 = List(Set("4.4.4.4"), Set("3.3.3.3.3.3"), Set("6.6.6")).map(_.map(sig))

  it should "merge per-bucket results into the union of distinct tilings" in:
    val res = BucketAssembly.enumerateBuckets(k1, maxV = 3, stateBudget = 2_000_000L, parallelism = 3)
    res.count shouldBe 3
    res.tilings.map(_.n).toSet shouldBe Set(1)
    // each driver key equals the single-bucket key (driver adds only fan-out + dedup, no behaviour change)
    for b <- k1 do
      val direct = BucketAssembly.enumerateBucket(b, maxV = 3).keys
      direct.subsetOf(res.byKey.keySet) shouldBe true

  it should "deduplicate identical buckets supplied twice" in:
    val res = BucketAssembly.enumerateBuckets(k1 ++ k1, maxV = 3, stateBudget = 2_000_000L, parallelism = 4)
    res.count shouldBe 3 // the duplicates collapse by canonical key
