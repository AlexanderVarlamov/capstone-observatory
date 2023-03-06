package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import observatory.Visualization.{colorsExample, visualize}

import scala.collection.parallel.CollectionConverters.given
//import scala.collection.parallel.CollectionConverters._
import math.{atan, sinh, toDegrees, toRadians, tan, cos, log, pow}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:
  import Visualization._
  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    val n = math.pow(2d, tile.zoom)
    val lon_deg = (tile.x.toDouble/n*360.0 - 180.0)
    val lat_rad = atan(sinh(Math.PI * (1-2*tile.y.toDouble/n)))
    val lat_deg = toDegrees(lat_rad)
    Location(lat_deg, lon_deg)


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage =
    val startSeq = generateLocations(tile).par
    val getTempRDD = startSeq.map((location:Location) => (location, predictTemperature(temperatures, location)))
    val getColorRDD = getTempRDD.map{
      (location:Location, temp:Temperature) => (location, interpolateColor(colors, temp))
    }
    val transformCoordAndColorRDD = getColorRDD.map {
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



  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    yearlyData.foreach{
      (year, data) =>
        val genTiles =
          for
            z <- 0 to 3
            x <- 0 to pow(2, z).toInt - 1
            y <- 0 to pow(2, z).toInt - 1
          yield Tile(x, y, z)
        val parTiles = genTiles.par
        parTiles.foreach(generateImage(year, _, data))
    }

  def generateLocations (tile: Tile):Seq[Location] =

    def degToNum(tile2:Tile, newZ:Int): (Int,Int) =
      val loc = tileLocation(tile2)
      val lat_rad = loc.lat.toRadians
      val n = math.pow(2d, newZ)
      val xtile = ((loc.lon+180.0)/360*n).round.toInt
      val ytile = ((1 - log(tan(toRadians(loc.lat)) + 1 / cos(toRadians(loc.lat))) / Math.PI)/2*n).round.toInt
      (xtile, ytile)


//    val newZoom = tile.zoom+8
    val newZoom = tile.zoom+8
    val (newX, newY) = degToNum(tile, newZoom)
    val locations =
      for
        x <- 0 to 255
        y <- 0 to 255
      yield tileLocation(new Tile(newX+x, newY+y, newZoom))
    locations.toSeq

  def transformToCoordinates256(loc: Location): (Double, Double) =
    val x = (((loc.lon + 180d)/360d)*256d)
    val y = (((90d - loc.lat)/360d)*256d)
    (x, y)

  def generateImages2015(temperatures:Iterable[(Location, Temperature)], zoomlevel:Int=0): Unit =
    val z = zoomlevel
    val genTiles =
      for
        x <- 0 to pow(2, z).toInt - 1
        y <- 0 to pow(2, z).toInt - 1
      yield Tile(x, y, z)
    genTiles
//      .par
      .foreach{
      tile2 =>
        println(s"generating tile ${tile2.x} , ${tile2.y}")
        val path =s"target/temperatures/2015/$zoomlevel/${tile2.x}-${tile2.y}.png"
        val img = tile(temperatures, colorsExample, tile2)
        img.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File(path))
    }



object interTest extends App {
  import Interaction._
  import Extraction._
//  val tileEx = Tile(0,0,0)

  val test = locateTemperatures(2015, "/stations.csv", "/2015.csv")
  val itr2 = locationYearlyAverageRecords(test)

  val itr = Seq(
    (Location(37.00, 78.2), 50.3),
    (Location(50.5, 50.5), 1.0),
    (Location(0, 0), -50.0)
  )

//  val img = tile(itr, colorsExample, tileEx)
//  val img = visualize(itr, colorsExample)
//  img.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File("target/2015.png"))
//
////  println(locs)
  println("generating average records done!")
  generateImages2015(itr2)
}