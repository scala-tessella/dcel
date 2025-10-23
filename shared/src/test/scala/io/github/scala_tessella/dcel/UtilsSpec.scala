package io.github.scala_tessella.dcel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.scala_tessella.dcel.Utils.*

class UtilsSpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "Utils.sequence on List[Either]"

  it should "accumulate rights preserving order" in {
    val xs: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    xs.sequence shouldBe Right(List(1, 2, 3))
  }

  it should "short-circuit on first right" in {
    val xs: List[Either[String, Int]] = List(Right(1), Left("err"), Right(3), Left("err2"))
    xs.sequence shouldBe Left("err2")
  }

  it should "work with empty list" in {
    val xs: List[Either[String, Int]] = Nil
    xs.sequence shouldBe Right(Nil)
  }

  it should "handle single left" in {
    val xs: List[Either[String, Int]] = List(Left("boom"))
    xs.sequence shouldBe Left("boom")
  }

  it should "handle single right" in {
    val xs: List[Either[String, Int]] = List(Right(42))
    xs.sequence shouldBe Right(List(42))
  }

  behavior of "Utils.traverse on Option"

  it should "map Some via function returning Right, wrapping in Some" in {
    val some                             = Some(5)
    val f: Int => Either[String, String] = i => Right(s"n=$i")
    some.traverse(f) shouldBe Right(Some("n=5"))
  }

  it should "propagate Left from function" in {
    val some                             = Some(5)
    val f: Int => Either[String, String] = _ => Left("bad")
    some.traverse(f) shouldBe Left("bad")
  }

  it should "return Right(None) for None regardless of function" in {
    val none: Option[Int]             = None
    val f: Int => Either[String, Int] = i => Right(i + 1)
    none.traverse(f) shouldBe Right(None)
  }

  behavior of "Utils.toMap2 on Seq"

  it should "map elements using provided function" in {
    val seq = Seq(1, 2, 3)
    seq.associate(_ * 10) shouldBe Map(1 -> 10, 2 -> 20, 3 -> 30)
  }

  it should "favor last occurrence as key when duplicates exist" in {
    val seq = Seq("a", "b", "a")
    // Map semantics keep the last value for duplicate key "a"
    seq.associate(_.toUpperCase) shouldBe Map("a" -> "A", "b" -> "B")
  }

  it should "work with empty sequence" in {
    val seq = Seq.empty[Int]
    seq.associate(_ + 1) shouldBe Map.empty
  }

  it should "support non-primitive types" in {
    case class K(i: Int)
    case class V(s: String)
    val k1  = K(1); val k2 = K(2)
    val res = Seq(k1, k2).associate(k => V(s"v${k.i}"))
    res shouldBe Map(k1 -> V("v1"), k2 -> V("v2"))
  }
