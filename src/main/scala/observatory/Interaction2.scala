package observatory

import Visualization.colorsExample as colors1
import Visualization2.colorsExample as colors2


/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface:

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] =
    val layer1 = Layer(LayerName.Temperatures, colors1, 1978 to 2015)
    val layer2 = Layer(LayerName.Deviations, colors2, 1991 to 2015)
    Seq(layer1, layer2)

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] =
    Signal(selectedLayer().bounds)

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] =

    Signal {
      if (selectedLayer().bounds.contains(sliderValue())) sliderValue()
      else if (selectedLayer().bounds.last < sliderValue()) selectedLayer().bounds.last
      else selectedLayer().bounds.head
    }
  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =
    Signal{s"target/${selectedLayer().layerName.id}/${selectedYear()}/{z}/{x}/{y}{r}.png"}

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =

    Signal(s"${selectedLayer().layerName} (${(selectedYear())})")



// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface:
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]

enum LayerName:
  case Temperatures, Deviations
  def id: String =
    this.match
      case Temperatures => "temperatures"
      case Deviations => "deviations"

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

object runInter extends App {
  import Interaction2.*
  val layers = availableLayers
  val slider = Signal(3010)
  val year = Signal.Var(1500)

  val curLayer = Signal.Var(layers(0))
  val capt = caption(curLayer, year)
  val yearSel = yearSelection(curLayer, year)
  val urlSel = layerUrlPattern(curLayer, yearSel)
  println(urlSel.currentValue)


  curLayer() = layers(1)
  year() = 2000


  println(urlSel.currentValue)




}