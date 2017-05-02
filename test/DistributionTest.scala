import org.junit.Test
import org.junit.Assert._
import vegas.spec.Spec.MarkEnums.Bar
import vegas.{Quant, Vegas}

class DistributionTest {

  @Test
  def uniformAndcategorial(): Unit = {
    val as = Uniform(true, false)
    println(as.sample(1000))
    val bs = Categorical(true->1, false->1)
    println(bs.sample(1000))
  }

  @Test
  def uniformAndcategorialDifferentWeights(): Unit = {
    val as = Uniform(true, false, false)
    println(as.sample(1000))
    val bs = Categorical(true->1, false->2)
    println(bs.sample(1000))
  }

  @Test
  def uniformfilter(): Unit = {
    val xs = Uniform(1,2,3,4,5,6).where(x => x != 3)
    val ys = xs.expand()
    println(ys.toSeq.slice(100, 101).toList)
  }

  @Test
  def obesity(): Unit = {

    object Weight extends Enumeration {
      type Weight = Value
      val Obese, Skinny = Value
    }

    object Food extends Enumeration {
      type Food = Value
      val Burger, Celery = Value
    }

    import Weight._
    import Food._

    val cdc: Distribution[Weight] =
      Categorical(Obese -> 2, Skinny -> 3)

    println(cdc.sample(100))

    def doctor(weight: Weight): Distribution[Food] = weight match {
      case Obese => Categorical(Burger -> 9, Celery -> 1)
      case Skinny => Categorical(Burger -> 3, Celery -> 7)
    }

    println(doctor(Obese).sample(100))
    println(doctor(Skinny).sample(100))

    println(cdc.where(_ == Obese).sample(100))
    println(cdc.where(_ == Skinny).sample(100))

    def predict(food: Food): Distribution[Weight] = {
      cdc.selectMany(doctor(_).where(_ == food))((weight, _) => weight)
    }

    println(predict(Burger).sample(1000))
    println(predict(Celery).sample(1000))

    def predict2(food: Food): Distribution[Weight] =
      cdc.selectMany(weight => doctor(weight))((weight,food) => (weight,food))
         .where({ case (weight, food_) => food_ == food })
         .select({ case (weight, food_) => weight })


    println(predict2(Burger).sample(1000))
    println(predict2(Celery).sample(1000))
  }

  @Test
  def factorTest(): Unit = {

    val xs = Categorical("A" -> 2, "B" -> 1, "C" -> 1)

    println(xs.sample(1000))

    val ys = xs.factor({
      case "A" => 1
      case "B" => 2
      case "C" => 0
    })

    println(ys.sample(1000))
  }

  // https://en.wikipedia.org/wiki/Bayesian_network
  @Test
  def sprinklerTest(): Unit = {

    object Weather extends Enumeration {
      type Weather = Value
      val Rain, Sunny = Value
    }
    import Weather._

    object Sprinkler extends Enumeration {
      type Sprinkler = Value
      val On, Off = Value
    }
    import Sprinkler._

    object Grass extends Enumeration {
      type Grass = Value
      val Wet, Dry = Value
    }
    import Grass._

    val rain: Distribution[Weather] =
      Categorical(Rain -> 20, Sunny -> 80)

    val sprinkler: (Weather) => Distribution[Sprinkler] = {
      case Sunny => Categorical(On -> 40, Off -> 60)
      case Rain  => Categorical(On -> 1,  Off -> 99)
    }

    val grass: ((Weather, Sprinkler)) => Distribution[Grass] = {
      case (Sunny, Off) => Categorical(Wet -> 0,  Dry -> 100)
      case (Rain,  Off) => Categorical(Wet -> 80, Dry -> 20)
      case (Sunny, On)  => Categorical(Wet -> 90, Dry -> 10)
      case (Rain,  On)  => Categorical(Wet -> 99, Dry -> 1)
    }

    val predict = (wet: Grass) =>
      rain.selectMany(sprinkler)({ case (r,s) => (r,s) })
          .selectMany(grass)({ case ((r,s),g) => (r,s,g) })
          .where({ case (r,s,g) => g == wet })
          .select({ case (r,s,g) =>  r })

    val p = predict(Wet)
    println(p.sample(10000))
  }

  @Test
  def normal(): Unit = {

    val xs = DiscreteGaussian(4, 1).selectMany(x => DiscreteGaussian(x, 1))((x,y)=>(x,y))
             .where({ case (x,y) => y == 3 })
             .select({ case (x,y) => x })
    println(xs.sample(1000))

  }

}
