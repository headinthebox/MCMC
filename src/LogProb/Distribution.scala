package LogProb

import java.lang.Math.{abs, exp, pow, rint, log}

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, ListMap}
import scala.util.Random


object Test {
  def main(args: Array[String]): Unit = {
    val xs = Uniform(1,2,3,4,5,6).condition(x => x != 3)
    val ys = xs.normalize(100000)
    println(ys)
  }
}

object Categorical {
  /**
    * @param ts Sequence of values->odds pairs
    */
  def apply[T](ts: (T, Int)*): Distribution[T] = {
    val random = new Random()
    val n = ts.size
    new Distribution[T] {
      def iterator: Iterator[(T, Double)] = {
        new Iterator[(T,Double)] {
          override def hasNext: Boolean = true
          override def next(): (T, Double) = {
            val (t, p) = ts(random.nextInt(n))
            (t, log(p))
          }
        }
      }
    }

  }
}

object Uniform {
  /**
    * @param ts Parameter list of values, with equal odds
    */
  def apply[T](ts: T*): Distribution[T] = {
    Categorical[T](ts.map(t => (t,1)): _*)
  }
}

object Gaussian {
  /**
    * @param μ: mean
    * @param σ: standard deviation
    */
  def apply(μ: Double, σ: Double): Distribution[Double] = {
    val random = new Random()
    new Distribution[Double] {
      def iterator: Iterator[(Double, Double)] = {
        new Iterator[(Double, Double)] {
          def hasNext: Boolean = true
          def next(): (Double, Double) = (random.nextGaussian()*σ+μ, log(1))
        }
      }
    }
  }

  def pdf(m: Double, v: Double, x: Double): Double =
    exp(-pow(x - m, 2) / (2 * v))
}

object DiscreteGaussian {

  def apply(μ: Int, σ: Int): Distribution[Int] =
    Bucket(Gaussian(μ, σ)).
      select(_.asInstanceOf[Int]).
      condition(v => -abs(v) < 3*σ)

  def pdf(m: Double, v: Double, x: Double, resolution: Int = 10): Int =
    (exp(-pow(x - m, 2) / (2 * v))*resolution).asInstanceOf[Int]
}



// empty distribution does not terminate

object Bucket {
  /**
    * @param dist Continuous distribution
    * @param resolution Number of decimal digits
    */
  def apply(dist: Distribution[Double], resolution: Int = 0): Distribution[Double] = {
    val d = math.pow(10.0, resolution)
    dist.select(v => rint(v*d)/d*1.0)
  }
}

trait Distribution[T] extends Iterable[(T, Double)] { that =>

  // http://timvieira.github.io/blog/post/2014/02/11/exp-normalize-trick/
  def normalize(n: Int) = {
    val kvs = ListMap(expand().toIterator.drop(n).next()._1.asInstanceOf[Distribution[T]].toSeq.sortWith((x,y) => x._2 > y._2): _*)
    val max = kvs.values.max
    val total = kvs.mapValues(v => exp(v-max)).values.sum
    kvs.mapValues(v => exp(v-max)/total)
  }

  def expand(): Distribution[Distribution[T]] = {

    new Distribution[Distribution[T]] {

      def iterator: Iterator[(Distribution[T], Double)] = {

        val outer = that.iterator
        var groups = HashMap[T, Double]()
        var max: Double = log(0) // -∞ => 0-probability

        new Iterator[(Distribution[T], Double)]{

          def hasNext: Boolean = true
          def next(): (Distribution[T], Double) = {
            val (t,v) = outer.next()
            var g: HashMap[T, Double] = groups.get(t) match {
              case None    => groups.updated(t,v)
              case Some(w) => groups.updated(t, if(v > w) v+log(1+exp(w-v)) else w+log(1+exp(v-w)))
            }
            groups = g
            new Distribution[T] { def iterator: Iterator[(T, Double)] = g.iterator } -> log(1)
          }
        }
      }

    }
  }

  def condition(p: T => Boolean): Distribution[T] = factor(t => if(p(t)) log(1) else log(0))

  def factor(p: T => Double): Distribution[T] = new Distribution[T] {
    def iterator: Iterator[(T, Double)] = {
      val outer = that.iterator
      new Iterator[(T, Double)] {
        def hasNext: Boolean = true
        @tailrec def next(): (T, Double) = {
          val (t, v) = outer.next()
          val w = p(t)
          if(w == log(0)) next() else (t, v+w)
        }
      }
    }
  }

  def select[S](f: T=>S): Distribution[S] = new Distribution[S] {
    def iterator: Iterator[(S, Double)] = {
      val outer = that.iterator
      new Iterator[(S, Double)] {
        def hasNext: Boolean = true
        def next(): (S, Double) = {
          val (t, v) = outer.next()
          (f(t), v)
        }
      }
    }
  }

  def then[S](f: T=> Distribution[S]): Distribution[S] =
    selectMany(f)((x,y) => y)

  def selectMany[S,R](f: T=> Distribution[S])(c: (T,S) => R): Distribution[R] = new Distribution[R] {
    def iterator: Iterator[(R, Double)] = {
      val outer = that.iterator
      new Iterator[(R,Double)] {
        def hasNext: Boolean = true
        def next(): (R, Double) = {
          val (t,v) = outer.next()
          val (s,w) = f(t).iterator.next()
          (c(t,s), v+w)
        }
      }
    }
  }
}
