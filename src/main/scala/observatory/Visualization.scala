package observatory

import ar.com.hjg.pngj.PngWriter
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits

import scala.collection.parallel.CollectionConverters._
import scala.math.{acos, cos, pow, sin, toRadians}
import ch.qos.logback.classic.{Level, Logger}
import com.sksamuel.scrimage.nio.PngWriter
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, SparkSession}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  val colorsExample:Seq[(Temperature, Color)] =
    Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )

  val generateLocations: Seq[Location] =
    val locations =
      for
        x <- (-180 until 180)
        y <- (-89 to 90)
      yield Location(y, x)

    locations.toSeq

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    val p = 6 //Степень интерполяции
    val rawRdd = temperatures.toSeq.par
    val rawDs = rawRdd
    val tempDistDs = rawDs.map((loc:Location, temp:Temperature) => (greatCircle(loc, location), temp))
    val closeStations = tempDistDs.filter((dist:Double, temp:Double) => (dist < 1.0))

    if (closeStations.size >= 1) {
      closeStations.head._2}
    else

      val tempDistDs2 = rawDs.map((loc:Location, temp:Temperature) => (pow(greatCircle(loc, location), -p), temp))
      val sumKoef:Double = tempDistDs2.map(dist => dist._1).reduce(_ + _)

      val tempDistWithWK: Temperature = tempDistDs2.map((koef: Double, temp:Double) => (koef*temp/sumKoef)).reduce(_ + _)
      tempDistWithWK


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    @tailrec
    def findRange(listOfPoints: List[(Temperature, Color)],
                  accMax:List[(Temperature, Color)]=List(),
                  accMin:List[(Temperature, Color)]=List()): List[(Temperature, Color)] =
      if (listOfPoints.isEmpty && !accMax.isEmpty && accMax.head._1 != value) accMin ++ accMax
      else if (listOfPoints.isEmpty && !accMax.isEmpty && accMax.head._1 == value) accMax
      else
        val currentPoint = listOfPoints.head
        val currentTemp = currentPoint._1
        if (!accMax.isEmpty) {
          if (accMax.head._1 == value) accMax
          else accMin.head +: accMax}
        else
          if (value <= currentTemp) findRange(listOfPoints.tail, currentPoint +: accMax, accMin)
          else
            findRange(listOfPoints.tail, accMax, List(currentPoint))


    def findNewValue(x1:Temperature, y1:Int, x2:Temperature, y2:Int): Int =
      val precise =((y1*(x2-value)+y2*(value - x1))/(x2-x1))
      precise.round.toInt

    val sortedPoints = points.toList.sortBy(_._1)
    val maxTemp = sortedPoints.map(x => x._1).reduce(_ max _)
    val minTemp = sortedPoints.map(x => x._1).reduce(_ min _)

    if (value > maxTemp) sortedPoints.reverse.head._2
    else if (value < minTemp) sortedPoints.head._2
    else
        val rangeOfTemp = findRange(sortedPoints)
        if (rangeOfTemp.size == 1) rangeOfTemp.head._2
        else
          val (temp1, color1) = rangeOfTemp.head
          val (temp2, color2) = rangeOfTemp.tail.head

          val red = findNewValue(temp1, color1.red, temp2, color2.red)
          val green = findNewValue(temp1, color1.green, temp2, color2.green)
          val blue = findNewValue(temp1, color1.blue, temp2, color2.blue)

          Color(red, green, blue)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =

    val startSeq = generateLocations.par
    val getTempRDD = startSeq.par.map((location:Location) => (location, predictTemperature(temperatures, location)))
    val getColorRDD = getTempRDD.map{
      (location:Location, temp:Temperature) => (location, interpolateColor(colors, temp))
    }
    val transformCoordAndColorRDD = getColorRDD.map{
      (location:Location, color:Color) =>
        val (x,y) = transformToCoordinates(location)
        val (r, g, b) = (color.red, color.green, color.blue)
        val a = 255
        Pixel(x,y, r, g, b, a)
    }
    val pixArrayUnsorted:Array[Pixel] = transformCoordAndColorRDD.toArray
    val pixArray = pixArrayUnsorted.sortBy(x => (x.y, x.x))
    val width = 360
    val height = 180
    ImmutableImage.wrapPixels(width, height, pixArray, ImageMetadata.empty)

  def greatCircle(loc1: Location, loc2: Location): Double =
    def deltaSigma(phi1: Double, phi2: Double, delta: Double): Double =
      acos(sin(phi1)*sin(phi2)+cos(phi1)*cos(phi2)*cos(delta))

    val earthRaduis = 6371.009

    val phi1 = (loc1.lat)
    val phi2 = (loc2.lat)
    val lamb1 = loc1.lon
    val lamb2 = loc2.lon
    val deltaLong = loc1.lon - loc2.lon

    val phi1Rad = toRadians(phi1)
    val phi2Rad = toRadians(phi2)
    val deltaLongRad = toRadians(deltaLong)


    val isAntipode = phi1 == -phi2 && (lamb1 == lamb2+180 || lamb1 == lamb2-180)
    val isTheSame = phi1 == phi2 && lamb1 == lamb2

    val delta = if (isTheSame) 0
                else if (isAntipode) Math.PI
                else deltaSigma(phi1Rad, phi2Rad, deltaLongRad)

    delta*earthRaduis

  def transformToCoordinates(loc: Location): (Int, Int) =
    val x = loc.lon.round.toInt + 180
    val y = 90 - loc.lat.round.toInt
    (x, y)


object vistest extends App {
  import Visualization._
  import Extraction._
//  val test = locateTemperatures(1978, "/stations.csv", "/1978.csv")
//  val itr2 = locationYearlyAverageRecords(test)
  val itr = Seq(
    (Location(37.00, 78.2), 50.3),
    (Location(50.5, 50.5), 1.0),
    (Location(0, 0), -50.0)
  )
  println(generateLocations.head)
  println(generateLocations.last)

  val img = visualize(itr, colorsExample)
  img.output(com.sksamuel.scrimage.nio.PngWriter.MinCompression, new java.io.File("target/1978.png"))
//  val xxx = interpolateColor(List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), value = -0.75)
//  println(xxx)

}