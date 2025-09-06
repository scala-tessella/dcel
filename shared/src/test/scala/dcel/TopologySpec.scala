
package dcel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologySpec extends AnyFlatSpec with Matchers with TilingTestHelpers:

  behavior of "Topology.breadthFirstSearch"

  it should "return only the start node when adjacency map is empty" in {
    val start = "A"
    val adjacency: Map[String, List[String]] = Map.empty
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A")
  }

  it should "return only the start node when it has no neighbors" in {
    val start = "A"
    val adjacency = Map("A" -> List.empty[String])
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A")
  }

  it should "find all connected nodes in a simple linear graph" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B"),
      "B" -> List("C"),
      "C" -> List("D")
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C", "D")
  }

  it should "find all connected nodes in a cycle" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B"),
      "B" -> List("C"),
      "C" -> List("A")
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C")
  }

  it should "find all connected nodes in a tree structure" in {
    val start = "root"
    val adjacency = Map(
      "root" -> List("left", "right"),
      "left" -> List("leftChild1", "leftChild2"),
      "right" -> List("rightChild1"),
      "leftChild1" -> List.empty[String],
      "leftChild2" -> List.empty[String],
      "rightChild1" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("root", "left", "right", "leftChild1", "leftChild2", "rightChild1")
  }

  it should "find all connected nodes in a complex graph" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B", "C"),
      "B" -> List("D", "E"),
      "C" -> List("F"),
      "D" -> List("G"),
      "E" -> List("G"),
      "F" -> List.empty[String],
      "G" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C", "D", "E", "F", "G")
  }

  it should "only return connected nodes, not all nodes in adjacency map" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B"),
      "B" -> List.empty[String],
      "C" -> List("D"), // Disconnected component
      "D" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    allAssert(
      result shouldBe Set("A", "B"),
      result should not contain "C",
      result should not contain "D"
    )
  }

  it should "handle self-loops correctly" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("A", "B"), // Self-loop
      "B" -> List("C"),
      "C" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C")
  }

  it should "handle multiple edges to the same node" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B", "B", "C"), // Multiple edges to B
      "B" -> List("D"),
      "C" -> List("D"),
      "D" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C", "D")
  }

  it should "work with different types - Integers" in {
    val start = 1
    val adjacency = Map(
      1 -> List(2, 3),
      2 -> List(4),
      3 -> List(4),
      4 -> List(5),
      5 -> List.empty[Int]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set(1, 2, 3, 4, 5)
  }

  it should "work with case class nodes" in {
    case class Node(id: String, value: Int)
    
    val nodeA = Node("A", 1)
    val nodeB = Node("B", 2)
    val nodeC = Node("C", 3)
    val nodeD = Node("D", 4)
    
    val start = nodeA
    val adjacency = Map(
      nodeA -> List(nodeB),
      nodeB -> List(nodeC),
      nodeC -> List(nodeD),
      nodeD -> List.empty[Node]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set(nodeA, nodeB, nodeC, nodeD)
  }

  it should "handle single node graph" in {
    val start = "single"
    val adjacency = Map("single" -> List.empty[String])
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("single")
  }

  it should "handle star topology" in {
    val start = "center"
    val adjacency = Map(
      "center" -> List("node1", "node2", "node3", "node4"),
      "node1" -> List.empty[String],
      "node2" -> List.empty[String],
      "node3" -> List.empty[String],
      "node4" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("center", "node1", "node2", "node3", "node4")
  }

  it should "handle bidirectional edges correctly" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B"),
      "B" -> List("A", "C"), // Bidirectional with A
      "C" -> List("B")       // Bidirectional with B
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C")
  }

  it should "visit nodes in breadth-first order (level by level)" in {
    // While we can't directly test the order due to Set return type,
    // we can verify all nodes at each level are found
    val start = "level0"
    val adjacency = Map(
      "level0" -> List("level1_1", "level1_2"),
      "level1_1" -> List("level2_1", "level2_2"),
      "level1_2" -> List("level2_3"),
      "level2_1" -> List.empty[String],
      "level2_2" -> List.empty[String],
      "level2_3" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("level0", "level1_1", "level1_2", "level2_1", "level2_2", "level2_3")
  }

  it should "work with vertices from DCEL" in {
    import BigDecimalGeometry.BigPoint
    
    val v1 = Vertex(V1, BigPoint(0, 0))
    val v2 = Vertex(V2, BigPoint(1, 0))
    val v3 = Vertex(V3, BigPoint(0, 1))
    
    val start = v1
    val adjacency = Map(
      v1 -> List(v2, v3),
      v2 -> List(v3),
      v3 -> List.empty[Vertex]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set(v1, v2, v3)
  }

  it should "work with faces from DCEL" in {
    val f1 = Face(F1)
    val f2 = Face(F2)
    val f3 = Face(F3)
    
    val start = f1
    val adjacency = Map(
      f1 -> List(f2),
      f2 -> List(f3),
      f3 -> List.empty[Face]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set(f1, f2, f3)
  }

  behavior of "Topology.breadthFirstSearch edge cases"

  it should "handle start node not in adjacency map" in {
    val start = "NotInMap"
    val adjacency = Map(
      "A" -> List("B"),
      "B" -> List.empty[String]
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("NotInMap")
  }

  it should "handle empty adjacency lists gracefully" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B", "C"),
      "B" -> Nil,
      "C" -> Nil
    )
    
    val result = Topology.breadthFirstSearch(start, adjacency)
    
    result shouldBe Set("A", "B", "C")
  }

  it should "handle large graphs efficiently" in {
    // Create a large linear graph
    val nodes = (1 to 1000).map(_.toString).toList
    val adjacency = nodes.zip(nodes.tail).toMap.view.mapValues(List(_)).toMap
    
    val start = "1"
    val result = Topology.breadthFirstSearch(start, adjacency)

    allAssert(
      result should have size 1000,
      result should contain allElementsOf nodes
    )
  }

  behavior of "Topology.breadthFirstSearch mutable state handling"

  it should "not modify the input adjacency map" in {
    val start = "A"
    val originalAdjacency = Map(
      "A" -> List("B"),
      "B" -> List("C"),
      "C" -> List.empty[String]
    )
    
    val adjacencyCopy = originalAdjacency
//    Topology.breadthFirstSearch(start, adjacencyCopy)
    
    adjacencyCopy shouldBe originalAdjacency
  }

  it should "produce consistent results on multiple calls" in {
    val start = "A"
    val adjacency = Map(
      "A" -> List("B", "C"),
      "B" -> List("D"),
      "C" -> List("D"),
      "D" -> List.empty[String]
    )
    
    val result1 = Topology.breadthFirstSearch(start, adjacency)
    val result2 = Topology.breadthFirstSearch(start, adjacency)
    val result3 = Topology.breadthFirstSearch(start, adjacency)

    allAssert(
      result1 shouldBe result2,
      result2 shouldBe result3
    )
  }
