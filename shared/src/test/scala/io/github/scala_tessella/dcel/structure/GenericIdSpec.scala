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
      vertexIdTester.fromStringSafe("V1").value shouldBe 1,
      faceIdTester.fromStringSafe("F999").value shouldBe 999,
      customIdTester.fromStringSafe("ID-123").value shouldBe 123
    )

  it should "throw IllegalArgumentException if the prefix is missing or incorrect" in:
    // Wrong prefix for the specific tester
    vertexIdTester.fromStringSafe("F1").left.value.message should include("Invalid id prefix: `F1`")

  it should "throw IllegalArgumentException if the string is too short" in:     
    vertexIdTester.fromStringSafe("V").left.value.message should include("Invalid numeric part in id: `V`")

  it should "throw IllegalArgumentException if the numeric part is not an integer" in:
    allAssert(
      vertexIdTester.fromStringSafe("Vabc").left.value.message should include("Invalid numeric part in id: `Vabc`"),
      vertexIdTester.fromStringSafe("V1.5").left.value.message should include("Invalid numeric part in id: `V1.5`")
    )

  it should "throw IllegalArgumentException for empty strings" in:
    vertexIdTester.fromStringSafe("").left.value.message should include("Invalid id prefix: ``")

  it should "throw IllegalArgumentException if there are leading spaces" in:     
    vertexIdTester.fromStringSafe(" V1").left.value.message should include("Invalid id prefix: ` V1`")

  it should "distinguish between IDs with different prefixes" in:
    val vId = "V5"
    allAssert(
      vertexIdTester.fromStringSafe(vId).value shouldBe 5, {
        // faceIdTester uses "F", so "V5" is invalid for it
        faceIdTester.fromStringSafe(vId).left.value.message should include("Invalid id prefix: `V5`")
      }
    )

  it should "correctly handle negative integers if they are used as IDs" in:
    vertexIdTester.fromStringSafe("V-1").value shouldBe -1

  it should "fail if the prefix is correct but the numeric part is missing" in:     
    customIdTester.fromStringSafe("ID-").left.value.message should include("Invalid")
