package observatory

import scala.collection.parallel.{CollectionConverters, ParMap, ParSeq}
import Visualization.*

import scala.collection.parallel.CollectionConverters.*
/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature =
    val gridLoc = generateGridLocations.par
    val tempParSeq:ParSeq[(GridLocation, Temperature)] = gridLoc map {
      (gridLocSingle:GridLocation) =>
        val loc:Location = gridLocSingle.toLocation
        val temp = predictTemperature(temperatures, loc)
        (gridLocSingle -> temp)
    }
    val tempMap = tempParSeq.toMap
    (grid:GridLocation) => tempMap.getOrElse(grid, -60.0)

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature =
    val grids = temperaturess.map(makeGrid(_))
    val gridLoc = generateGridLocations.par
    val temps = gridLoc map {
      singleGrid => (singleGrid, {
        val iterTemp = grids map {
       (singleYearfunc: GridLocation => Temperature) =>
         singleYearfunc(singleGrid)
      }
        iterTemp.reduce(_+_)/grids.size}
      )
    }
    val tempMap = temps.toMap

    (grid:GridLocation) => tempMap(grid)

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
    val mkGrid = makeGrid(temperatures)
    (grid:GridLocation) =>
      val actualTemp = mkGrid(grid)
      val avgTemp = normals(grid)
      actualTemp - avgTemp

  val generateGridLocations: Seq[GridLocation] =
    val locations =
      for
        x <- (-180 until 180)
        y <- (-89 to 90)
      yield GridLocation(y, x)

    locations.toSeq

object manipTest extends App {
  import Manipulation.*
  val itr1 = Seq(
    (Location(37.00, 78.2), 50.3),
    (Location(50.5, 50.5), 1.0),
    (Location(0, 0), -50.0)
  )
  val itr2 = Seq(
    (Location(87.00, 78.2), 10.3),
    (Location(50.5, -50.5), 11.0),
    (Location(0, 0), -50.0)
  )

  val seqqq = Seq(itr1, itr2)
  val grid = GridLocation(80, -0)

  val temp = average(seqqq)(grid)
  println(temp)

}
