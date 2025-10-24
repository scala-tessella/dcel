package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.Tree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TreeSpec extends AnyFlatSpec with should.Matchers {

  val tree_ABCDEFG: Tree[Char] =
    Branch(
      'A',
      List(
        Branch(
          'B',
          List(
            Leaf('D'),
            Leaf('E')
          )
        ),
        Branch(
          'C',
          List(
            Leaf('F'),
            Leaf('G')
          )
        )
      )
    )

  "A tree" must "have a size" in {
    tree_ABCDEFG.size shouldBe 7
  }

  it must "have a size of leaves" in {
    tree_ABCDEFG.sizeLeaves shouldBe 4
  }

  it can "contain a value" in {
    tree_ABCDEFG.contains('B') shouldBe
      true
    tree_ABCDEFG.contains('F') shouldBe
      true
    tree_ABCDEFG.contains('Z') shouldBe
      false
  }

  it can "be flattened" in {
    tree_ABCDEFG.flatten.mkString shouldBe
      "ABDECFG"
  }

  it can "have its leaves listed" in {
    tree_ABCDEFG.flattenLeaves shouldBe
      List('D', 'E', 'F', 'G')
  }

  val tree_0123456: Tree[Int] =
    Branch(
      0,
      List(
        Branch(
          1,
          List(
            Leaf(3),
            Leaf(4)
          )
        ),
        Branch(
          2,
          List(
            Leaf(5),
            Leaf(6)
          )
        )
      )
    )

  it can "have its leaves mapped" in {
    tree_ABCDEFG.map(_.toInt - 65) shouldBe
      tree_0123456
  }

  it can "have them mapped via unfold" in {
    Tree.map(tree_ABCDEFG, _.toInt - 65) shouldBe
      tree_0123456
  }

  it can "be exported to a DOT graph with labels" in {
    tree_ABCDEFG.toDOT(_.toString) shouldBe
      """graph G {
        |1 [label="A"]
        |1 -- 2
        |2 [label="B"]
        |2 -- 3
        |3 [label="D"]
        |2 -- 4
        |4 [label="E"]
        |1 -- 5
        |5 [label="C"]
        |5 -- 6
        |6 [label="F"]
        |5 -- 7
        |7 [label="G"]
        |}""".stripMargin
  }

  "Another tree" can "be shrunk" in {
    tree_0123456.shrink(_.sum) shouldBe
      Branch(
        3,
        List(
          Leaf(7),
          Leaf(11)
        )
      )
    tree_0123456.shrinkAll(_.sum) shouldBe
      Leaf(18)
  }

  it can "have only its leaves mapped" in {
    tree_0123456.mapLeaves(_ + 1) shouldBe
      Branch(
        0,
        List(
          Branch(
            1,
            List(
              Leaf(4),
              Leaf(5)
            )
          ),
          Branch(
            2,
            List(
              Leaf(6),
              Leaf(7)
            )
          )
        )
      )
  }

  it can "have all its values folded into one" in {
    tree_0123456.foldValues(_.sum) shouldBe
      21
  }

  it can "be exported to a DOT graph" in {
    tree_ABCDEFG.toDOT() shouldBe
      """graph G {
        |1
        |1 -- 2
        |2
        |2 -- 3
        |3
        |2 -- 4
        |4
        |1 -- 5
        |5
        |5 -- 6
        |6
        |5 -- 7
        |7
        |}""".stripMargin
  }

  "A tree" can "be created by fill" in {
    Tree.fill(2)('A') shouldBe
      Branch(
        'A',
        List(
          Branch(
            'A',
            List(
              Leaf('A'),
              Leaf('A')
            )
          ),
          Branch(
            'A',
            List(
              Leaf('A'),
              Leaf('A')
            )
          )
        )
      )
  }

  "A different tree" can "be folded" in {
    Branch(
      1,
      List(
        Leaf(3),
        Leaf(4)
      )
    ).foldValues(_.sum) shouldBe
      8
  }

}
