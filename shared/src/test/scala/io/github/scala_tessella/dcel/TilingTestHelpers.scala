package io.github.scala_tessella.dcel

import io.github.scala_tessella.dcel.geometry.RegularPolygon
import io.github.scala_tessella.dcel.structure.{FaceId, VertexId}
import org.scalatest.Assertions.succeed
import org.scalatest.{Assertion, EitherValues}

import scala.xml.{Elem, XML}
import scala.io.Source.fromFile

/** A trait for test classes with helper methods to create tiling fixtures. */
trait TilingTestHelpers extends EitherValues:

  object allAssert:
    def apply(assertions: Assertion*): Assertion = succeed

  def emptyTiling: Tiling =
    Tiling.empty

  /** Tiling with a single triangle <img src="file:../../../../../resources/triangle.svg"/>
    */
  def triangle: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(3))

  /** Tiling with a single square <img src="file:../../../../../resources/square.svg"/>
    */
  def square: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(4))

  /** Tiling with a single rhombus <img src="file:../../../../../resources/rhombus.svg"/>
    */
  def rhombus: Tiling =
    TilingBuilder.createSimplePolygon(60, 120, 60, 120).value

  /** Tiling with a single regular hexagon <img src="file:../../../../../resources/hexagon.svg"/>
    */
  def hexagon: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(6))

  /** Tiling with a single regular dodecagon <img src="file:../../../../../resources/dodecagon.svg"/>
    */
  def dodecagon: Tiling =
    TilingBuilder.createRegularPolygon(RegularPolygon(12))

  val V1: VertexId = VertexId(1)
  val V2: VertexId = VertexId(2)
  val V3: VertexId = VertexId(3)
  val V4: VertexId = VertexId(4)
  val V5: VertexId = VertexId(5)
  val V6: VertexId = VertexId(6)

  val F0: FaceId = FaceId.outerId
  val F1: FaceId = FaceId.firstInnerId
  val F2: FaceId = FaceId(2)
  val F3: FaceId = FaceId(3)

  private def saveFile(elem: Elem, filename: String)(extension: String): Unit =
    XML.save(
      s"shared/src/test/resources/$filename.$extension",
      elem,
      "UTF-8",
      xmlDecl = extension match
        case "xml"  => true
        case "svg"  => true
        case "html" => false
        case _      => throw new Error
    )

  def saveFileXML(elem: Elem, filename: String): Unit =
    saveFile(elem, filename)("xml")

  def saveFileSVG(elem: Elem, filename: String): Unit =
    saveFile(elem, filename)("svg")

  def saveFileHTML(elem: Elem, filename: String): Unit =
    saveFile(elem, filename)("html")

  def loadFile(filename: String): String =
    val source = fromFile(s"shared/src/test/resources/$filename", "UTF-8")
    try source.mkString
    finally source.close()

  extension (tree: Tree[List[VertexId]])

    def orderedForComparison: Tree[List[VertexId]] =
      tree
        .map: vertexIds =>
          vertexIds.sorted
        .transformChildren: children =>
          children.sortBy: child =>
            child.value.head
