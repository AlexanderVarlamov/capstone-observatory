package observatory

import Visualization._

trait VisualizationTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  test("greatCircle works with the same locations") {
    val loc1 = Location(43.0, 87.5)
    val loc2 = Location(43.0, 87.5)
    val distance = greatCircle(loc1, loc2)

    assertEquals(distance, 0.0, "There should be zero")
  }

  test("greatCircle works with the same antipodes") {
    val loc1 = Location(43.0, 87.5)
    val loc2 = Location(-43.0, 87.5-180)
    val distance = greatCircle(loc1, loc2).round

    assertEquals(distance, 20015L, "There should be 20015")
  }

  test("greatCircle should measure Moscow-Penza distance") {
    val Penza = Location(53.20, 45.0)
    val Moscow = Location(55.75, 37.62)
    val distance = greatCircle(Penza, Moscow).round

    assertEquals(distance, 554L, "There should be 554")
  }

  test("predictTemperature predits!") {
    val itr = Seq(
      (Location(37.00, 78.2), 27.3),
      (Location(50.5, 50.5), 1.0),
      (Location(-40.0, -40.0), -20.0)
    )
    val loc = Location(-37, -12)
    val result = ((10*predictTemperature(itr, loc)).round)/10.0

    assertEquals(result, -19.4, "There shuld be -17.4")

  }

  test("predictTemperature works on the borders"){
    val col1 = interpolateColor(colorsExample, -60)
    val col2 = interpolateColor(colorsExample, 60)

    assertEquals((col1,col2), (Color(0, 0, 0), Color(255, 255, 255)), "It should work on the edges!")

  }

  test("predictTemperature works outside ranges") {

    val col1 = interpolateColor(colorsExample, -70)
    val col2 = interpolateColor(colorsExample, 70)

    assertEquals((col1,col2), (Color(0, 0, 0), Color(255, 255, 255)), "It should work outside the edges!")
  }

  test("transformCoordinates"){
    val (x, y) = transformToCoordinates(Location(-89, -180))

    println(x)
    println(y)
  }