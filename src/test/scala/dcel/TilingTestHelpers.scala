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

  /** A tiling with a single rhombus
   * <img src="file:../../resources/rhombus.svg"/>
   **/
  def rhombus: TilingDCEL =
    TilingBuilder.createSimplePolygon(60, 120, 60, 120).value

  /** A tiling with a single regular hexagon
   * <img src="file:../../resources/hexagon.svg"/>
   **/
  def hexagon: TilingDCEL =
    TilingBuilder.createRegularPolygon(6).value
