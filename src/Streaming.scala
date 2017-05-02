import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.chart.XYChart.{Data, Series}
import javafx.scene.chart.{LineChart, NumberAxis, ScatterChart, XYChart}
import javafx.stage.Stage

import scala.util.Random
//http://koaning.io/bayesian-propto-streaming-algorithms.html

object Streaming {

  def main(args: Array[String]): Unit = {

    val xs = Range(1,25)

    println(xs.map(_ => Random.nextDouble()*2))

    Application.launch(classOf[Plot])
  }
}

// If x is a sample from a mean 0 and variance 1 distribution then
// σx+μ is a sample with mean μ and variance σ^2.

class Plot extends Application {

  override def start(primaryStage: Stage): Unit = {

    val series = new XYChart.Series[Number, Number]()

    val xs = Range(1,25).map(_ => Random.nextDouble()*2)
    val xys: Seq[Data[Number, Number]] = xs.map(x => new XYChart.Data[Number, Number](
      x,
      2.5 + 3.5*x+Random.nextGaussian()*0.3))

    xys.forall(series.getData.add(_))

    val sc = new LineChart(new NumberAxis(-0.5, 2.5, 0.5), new NumberAxis(0, 10, 1))

    sc.getData.setAll(series)

    val scene  = new Scene(sc, 300, 250)

    primaryStage.setScene(scene)
    primaryStage.show

    def next(x: Double, y: Double)(slope: Double, intercept: Double) =
      Random.nextGaussian()*0.3 + (intercept + slope*x - y)

    var slope = Random.nextDouble()
    var intercept = Random.nextDouble()

    xys.foreach(xy => {

    })

  }
}
