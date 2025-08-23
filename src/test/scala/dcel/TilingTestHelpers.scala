package io.github.scala_tessella.dcel

import org.scalatest.EitherValues

/** A trait for test classes with helper methods to create tiling fixtures. */
trait TilingTestHelpers extends EitherValues:

  /** A tiling with a single triangle
   * <img src="file:../../resources/triangle.svg"/>
   **/
  def triangle: TilingDCEL =
    TilingBuilder.createRegularPolygon(3).value

  /** A tiling with a single square
   * <img src="file:../../resources/square.svg"/>
   **/
  def square: TilingDCEL =
    TilingBuilder.createRegularPolygon(4).value

  /** A tiling with a single hexagon
   * <img src="file:../../resources/hexagon.svg"/>
   **/
  def hexagon: TilingDCEL =
    TilingBuilder.createRegularPolygon(6).value
