package io.github.scala_tessella.dcel.structure

import io.github.scala_tessella.dcel.TilingTestHelpers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenericIdSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  // Concrete implementation for testing the trait
  case class TestId(prefix: String) extends GenericId

  val vertexIdTester = TestId("V")
  val faceIdTester   = TestId("F")
  val customIdTester = TestId("ID-")

  behavior of "GenericId.prefixedString"

  it should "format an integer with the correct prefix" in:
    allAssert(
      vertexIdTester.prefixedString(1) shouldBe "V1",
      faceIdTester.prefixedString(42) shouldBe "F42",
      customIdTester.prefixedString(0) shouldBe "ID-0"
    )

  behavior of "GenericId.fromStringSafe"

  it should "successfully parse a valid prefixed string" in:
    allAssert(
      vertexIdTester.fromStringSafe("V1") shouldBe 1,
      faceIdTester.fromStringSafe("F999") shouldBe 999,
      customIdTester.fromStringSafe("ID-123") shouldBe 123
    )

  it should "throw IllegalArgumentException if the prefix is missing or incorrect" in:
    // Wrong prefix for the specific tester
    val caught = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe("F1")
    caught.getMessage should include("Invalid id: `F1`")

  it should "throw IllegalArgumentException if the string is too short" in:
    val caught = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe("V")
    caught.getMessage should include("Invalid id: `V`")

  it should "throw IllegalArgumentException if the numeric part is not an integer" in:
    val caught1 = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe("Vabc")
    val caught2 = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe("V1.5")
    allAssert(
      caught1.getMessage should include("Invalid id: `Vabc`"),
      caught2.getMessage should include("Invalid id: `V1.5`")
    )

  it should "throw IllegalArgumentException for empty strings" in:
    val caught = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe("")
    caught.getMessage should include("Invalid id: ``")

  it should "throw IllegalArgumentException if there are leading spaces" in:
    val caught = intercept[IllegalArgumentException]:
      vertexIdTester.fromStringSafe(" V1")
    caught.getMessage should include("Invalid id: ` V1`")

  it should "distinguish between IDs with different prefixes" in:
    val vId = "V5"
    allAssert(
      vertexIdTester.fromStringSafe(vId) shouldBe 5, {
        // faceIdTester uses "F", so "V5" is invalid for it
        val caught = intercept[IllegalArgumentException]:
          faceIdTester.fromStringSafe(vId)
        caught.getMessage should include("Invalid id: `V5`")
      }
    )
