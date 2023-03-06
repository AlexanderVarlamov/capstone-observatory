package observatory

import Extraction._
import java.time.LocalDate

trait ExtractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  test("locateTemperatures works fine") {
    val test = locateTemperatures(2015, "/test_loc.csv", "/test_temp.csv").toSeq.toSet
    val shouldBe = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    ).toSet
    assertEquals(test, shouldBe, "test should be equal to sample")
  }

  test("locationYearlyAverageRecords works good") {
    val iter = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val test = locationYearlyAverageRecords(iter)
    val shouldBe = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    assertEquals(test, shouldBe, "test should be equal to sample")
  }
  
