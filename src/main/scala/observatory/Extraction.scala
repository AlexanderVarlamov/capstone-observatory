package observatory

import java.time.LocalDate
import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, Dataset, SparkSession}
import org.apache.spark.sql.functions.{avg, col, udf}
import org.apache.spark.sql.types.DataTypes
import org.apache.spark.sql.types.*


/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:
  import ch.qos.logback.classic.{Level,Logger}
  import org.slf4j.LoggerFactory
  
  LoggerFactory
    .getLogger("org.apache.spark")
    .asInstanceOf[Logger]
    .setLevel(Level.WARN)
  
  // For a more general setting :
  LoggerFactory
    .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    .asInstanceOf[Logger]
    .setLevel(Level.WARN)
  implicit val spark: SparkSession = SparkSession.builder().appName("meteoTask").master("local").getOrCreate()
  spark.sparkContext.setLogLevel("ERROR")
  import spark.implicits._
  import scala3encoders.given

 

/**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
    val stations = readStations(stationsFile)
    val temperatures = temperRead(temperaturesFile)
    val joined = stations.join(temperatures, "STN_identifier_WBAN")
    val joinedTuple = joined.map(row => (year, row.getAs[Int](3), row.getAs[Int](4), row.getAs[Double](1), row.getAs[Double](2), row.getAs[Temperature](5))).rdd
    val result = joinedTuple.map(row => (LocalDate.of(row._1, row._2, row._3), Location(row._4, row._5), row._6.asInstanceOf[Temperature])).collect()
    result

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    val recordsRDD: RDD[(LocalDate, Location, Temperature)] = spark.sparkContext.parallelize(records.toSeq)
    val recordsExploded = recordsRDD.map {
      (date:LocalDate, location: Location, temperature: Temperature) =>
        (location.lat, location.lon, temperature)
    }
    val schema = "lat Double, lon Double, temp Double"
    val schemaStruct = StructType.fromDDL(schema)
    val dfRaw: DataFrame = recordsExploded.toDF("lat", "lon", "temp")
    val dfAvg: DataFrame = dfRaw.groupBy("lat", "lon")
      .agg(avg("temp").as("avg_temp"))
    val rddAvg = dfAvg.rdd.map {
      row => (Location(row.getAs[Double](0), row.getAs[Double](1)), row.getAs[Double](2))
    }
    rddAvg.collect()


  val stationFileExample = "src/main/resources/stations.csv"
  val tempFileExample = "src/main/resources/1977.csv"


  def readStations(stationPath: String):Dataset[StationRow] =
    val schema = "STN_identifier STRING, WBAN_identifier String, Latitude Double, Longitude Double"
    val lines =
      scala.io.Source.fromInputStream(getClass.getResourceAsStream(stationPath), "utf-8")
        .getLines()
        .toList
    val dataset: Dataset[String] = spark.sparkContext.parallelize(lines).toDS
    val stations = spark.read.schema(schema).csv(dataset)
    val filterNull =
      """
        |Latitude is NOT NULL AND
        |Longitude is NOT NULL
        |""".stripMargin
    val statitonFiltered: DataFrame = stations.filter(filterNull)
    val stationDataset = statitonFiltered.map{
      row => StationRow(
        row.getAs[String](0)+row.getAs[String](1),
        row.getAs[Double](2),
        row.getAs[Double](3)
      )
    }.as[StationRow]
    stationDataset



  def temperRead(tempPath: String): Dataset[TemperRow] =

    val farToCels  = (far:Double) => ((10.0*(5.0*(far - 32.0)/9.0)).round)/10.0
    val schema = "STN_identifier STRING, WBAN_identifier STRING, Month INT, Day INT,Temperature_F Double"
    val filterNull = "Temperature_F IS NOT NULL AND Temperature_F !=  9999.9"

    val lines =
      scala.io.Source.fromInputStream(getClass.getResourceAsStream(tempPath), "utf-8")
        .getLines()
        .toList
    val dataset: Dataset[String] = spark.sparkContext.parallelize(lines).toDS

    val temperF = spark.read.schema(schema)
      .csv(dataset)
      .filter(filterNull)

    val temperDataSet = temperF.map {
      Row =>
        TemperRow(

          Row.getAs[String](0)+ Row.getAs[String](1),
          Row.getAs[Int](2),
          Row.getAs[Int](3),
          farToCels(Row.getAs[Double](4))
        )
    }.as[TemperRow]
    temperDataSet



object testExtraction extends App {
  import Extraction._
  val test = locateTemperatures(1977, "/test_loc.csv", "/test_temp.csv")
  println(test)
}

