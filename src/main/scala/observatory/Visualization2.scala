package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import Interaction.{generateLocations, transformToCoordinates256}
import scala.collection.parallel.CollectionConverters.given
import Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  val colorsExample:Seq[(Temperature, Color)] =
    Seq(
      (7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255))

    )
  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
    val (x,y) = (point.x, point.y)
    d00*(1-x)*(1-y) + d10*x*(1-y) + d01*(1-x)*y + d11*x*y

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage =
    val cachedGrid =
      for gridLocation <- generateGridLocations yield (gridLocation -> grid(gridLocation))

    val gridMap = cachedGrid.toMap
    val locationAndTemp  = generateLocations(tile).par
      .map((loc:Location) => (loc, locToBilin(loc, gridMap)))
      .map((loc:Location, temp:Temperature) => (loc, interpolateColor(colors, temp)))
    val transformCoordAndColorRDD = locationAndTemp.map {
      (location: Location, color: Color) =>
        val (x, y) = transformToCoordinates256(location)
        val (r, g, b) = (color.red, color.green, color.blue)
        val a = 127
        (x, y, r, g, b, a)
    }
    val pixArray:Array[Pixel] = transformCoordAndColorRDD
      //      .flatMap((x, y, r, g, b, a) => Array((2*x, 2*y, r, g, b, a),
      //        (2*x+1, 2*y, r, g, b, a),
      //        (2*x, 2*y+1, r, g, b, a),
      //        (2*x+1, 2*y+1, r, g, b, a)))
      .toArray
      .sortBy((x, y, r, g, b, a) => (y,x))
      .map((x, y, r, g, b, a) => Pixel(x.toInt, y.toInt, r, g, b, a))
    val width = 256
    val height = 256
    ImmutableImage.wrapPixels(width, height, pixArray, ImageMetadata.empty)

  def locToBilin(loc:Location, grid:  Map[GridLocation, Temperature]):Temperature =
    val point = CellPoint(loc.lon - loc.lon.floor.toInt, loc.lat - loc.lat.floor.toInt)
    val d00 = grid(loc.gridTopLeft)
    val d01 = grid(loc.gridBottomLeft)
    val d10 = grid(loc.gridTopRight)
    val d11 = grid(loc.gridBottomRight)
    bilinearInterpolation(point, d00, d01, d10, d11)

  val generateGridLocations: Seq[GridLocation] =
      for
        x <- (-180 until 180)
        y <- (-89 to 90)
      yield GridLocation(y, x)


object vis2test extends App   {
//  import Visualization._
//  import Visualization2._
//  import Interaction._
//  import Extraction._
//  import Manipulation.*
//
////  val ctrlP = CellPoint(0.5, 0.5)
////  val (t1, t2, t3, t4) = (32, 45, 67, 86)
////  println(bilinearInterpolation(ctrlP, t1, t2, t3, t4))
//
//  val tileEx = Tile(0,0,0)
//
//  val test = locateTemperatures(1978, "/stations.csv", "/1978.csv")
//  val normals = locationYearlyAverageRecords(test)
//  val img1978 = visualize(normals, Visualization.colorsExample)
//  img1978.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File("target/1978.png"))
//  println("1978 is done")
//
//  val test2 = locateTemperatures(2015, "/stations.csv", "/2015.csv")
//  val avg2015 = locationYearlyAverageRecords(test2)
//  val img2015 = visualize(avg2015, Visualization.colorsExample)
//  img2015.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File("target/2015.png"))
//  println("2015 is done")
//
//  val g1 = makeGrid(normals)
//  println("grid is done")
//  val g = deviation(avg2015, g1)
//  println("deviation is done")
//  val img = visualizeGrid(g, Visualization2.colorsExample, tileEx)
//  println("grid image is done")
//  img.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File("target/grid.png"))
}