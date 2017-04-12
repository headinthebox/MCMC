import scala.collection.immutable.HashMap
import scala.util.Random

//TODO: move variable capture to the outermost possible point

object Uniform {
  def apply[T](ts: T*): Distribution[T] = {
    Categorical[T](ts.map(t => (t,1)): _*)
  }
}

object Categorical {
  def apply[T](ts: (T, Int)*): Distribution[T] = {

    new Distribution[T] {
      val random = new Random()
      val n = ts.size
      def iterator: Iterator[(T, Int)] = {
        new Iterator[(T,Int)] {
          override def hasNext: Boolean = true
          override def next(): (T, Int) =  {
            ts(random.nextInt(n))
          }
        }
      }
    }

  }
}

// empty distribution does not terminate

trait Distribution[T] extends Iterable[(T, Int)] { that =>

  def normalize(n: Int): Map[T, Double] = {
    val kvs = expand().toIterator.drop(n).next()._1.asInstanceOf[Distribution[T]].toMap
    val total = kvs.values.sum*1.0
    kvs.mapValues(v => v/total)
  }

  def expand(): Distribution[Distribution[T]] = {

    new Distribution[Distribution[T]] {

      def iterator: Iterator[(Distribution[T], Int)] = {

        val outer = that.iterator
        var groups = HashMap[T, Int]()

        new Iterator[(Distribution[T], Int)]{

          def hasNext: Boolean = outer.hasNext
          def next(): (Distribution[T], Int) = {
            val (k,v) = outer.next()
            val g =  groups.updated(k, groups.getOrElse(k,0)+v)
            groups = g
            (new Distribution[T] { override def iterator: Iterator[(T, Int)] = g.iterator },1)
          }
        }
      }

    }
  }

  def condition(p: T => Boolean): Distribution[T] = {
    factor(t => if(p(t)) 1 else 0)
  }

  def factor(p: T => Int): Distribution[T] = {
    new Distribution[T] {
      def iterator: Iterator[(T, Int)] = {
        val outer = that.iterator // random stream of samples, so can share across all iterators
        new Iterator[(T, Int)] {
          def hasNext: Boolean = true
          def next(): (T, Int) = {
            val (k, v) = outer.next()
            (k, p(k) * v)
          }
        }.filter(kv => kv._2 != 0)
      }
    }
  }

  def select[S](f: T=>S): Distribution[S] = selectMany(t => Uniform(f(t)))((a,b) => b)


  def selectMany[S,R](f: T=> Distribution[S])(c: (T,S) => R): Distribution[R] = {

    new Distribution[R] {
      def iterator: Iterator[(R, Int)] = {
        val outer = that.iterator
        new Iterator[(R,Int)] {
          def hasNext: Boolean = true
          def next(): (R, Int) = {
            val (t,v) = outer.next()
            val (s, w) = f(t).iterator.next()
            (c(t,s), v*w)
          }
        }
      }
    }
  }
}

object Weight extends Enumeration {
  val Obese, Skinny = Value
}

object Food extends Enumeration {
  val Burger, Celery = Value
}


object Test {
  def main(args: Array[String]): Unit = {

    val cdc: Distribution[Weight.Value] =
      Categorical(Weight.Obese -> 2, Weight.Skinny -> 3)

    println(cdc.normalize(100))

    val doctor: (Weight.Value) => Distribution[Food.Value] = {
      case Weight.Obese => Categorical(Food.Burger -> 9, Food.Celery -> 1)
      case Weight.Skinny => Categorical(Food.Burger -> 3, Food.Celery -> 7)
    }

    println(doctor(Weight.Obese).normalize(100))
    println(doctor(Weight.Skinny).normalize(100))

    println(cdc.condition(_ == Weight.Obese).normalize(100))
    println(cdc.condition(_ == Weight.Skinny).normalize(100))

    val predict = (food: Food.Value) => {
      cdc.selectMany(weight => doctor(weight).condition(_ == food))((weight, food) => weight)
    }

    println(predict(Food.Burger).normalize(100))
    println(predict(Food.Celery).normalize(100))
  }
}

