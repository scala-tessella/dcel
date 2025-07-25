package io.github.scala_tessella
package dcel

import BigDecimalGeometry.*
import Polygon.RegularPolygon
import spire.implicits.*

import scala.collection.mutable.ListBuffer

object TilingAddition:

  extension (tilingDCEL: TilingDCEL)

    def addRegularPolygon(sides: Int, onEdgeStartingWithVertexId: String): Either[String, TilingDCEL] =
      for
        _ <- TilingBuilder.validateSides(sides, "regular")
        boundaryEdges <- tilingDCEL.getBoundaryEdges
        edgeToBuildOn <- boundaryEdges.find(_.origin.id == onEdgeStartingWithVertexId)
          .toRight(s"Edge starting with vertex $onEdgeStartingWithVertexId not found on the boundary.")
        v_start = edgeToBuildOn.origin
        v_end <- edgeToBuildOn.destination.toRight("Edge has no destination vertex.")
      yield
        ???
