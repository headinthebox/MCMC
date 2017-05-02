import java.lang.Math._

import vegas.{Line, Bar, Quant, Vegas}

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, ListMap}
import scala.util.Random


object Categorical {
  /**
    * @param ts Sequence of values->odds pairs
    */
  def apply[T](ts: (T, Int)*): Distribution[T] = {
    val random = new Random()
    val n = ts.size
    new Distribution[T] {
      def iterator: Iterator[(T, Int)] = {
        new Iterator[(T,Int)] {
          def hasNext: Boolean = true
          def next(): (T, Int) = ts(random.nextInt(n))
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
      def iterator: Iterator[(Double, Int)] = {
        new Iterator[(Double, Int)] {
          def hasNext: Boolean = true
          def next(): (Double, Int) = (random.nextGaussian()*σ+μ, 1)
        }
      }
    }
  }

  def pdf(m: Double, v: Double, x: Double): Double = exp(-pow(x - m, 2) / (2 * v))
}

object DiscreteGaussian {

  def apply(μ: Int, σ: Int): Distribution[Int] =
    Bucket(Gaussian(μ, σ)).select(_.asInstanceOf[Int]).where(v => -abs(v) < 3*σ)

  def pdf(m: Double, v: Double, x: Double, resolution: Int = 10): Int = (exp(-pow(x - m, 2) / (2 * v))*resolution).asInstanceOf[Int]
}

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

trait Distribution[T] extends Iterable[(T, Int)] { that =>

  /*
  val kvs: Map<T, Int> = expand().asSequence().drop(n).iterator().next().first.sortedByDescending { kv -> kv.second }.toMap()
        val total: Double = kvs.values.sum()*1.0
        return kvs.mapValues { kv -> kv.value / total }
   */
  def sample(n: Int): Map[T, Double] = {
    val kvs = expand().toSeq.drop(n).head._1.toSeq.sortWith((x,y) => x._2 > y._2).toMap
    val total = kvs.values.sum*1.0 // can keep track in inner
    kvs.mapValues(v => v/total)
  }

  def expand(): Distribution[Distribution[T]] = {

    new Distribution[Distribution[T]] {

      def iterator: Iterator[(Distribution[T], Int)] = {

        val outer = that.iterator
        var groups = HashMap[T, Int]()

        new Iterator[(Distribution[T], Int)]{

          def hasNext: Boolean = true
          def next(): (Distribution[T], Int) = {
            val (k,v) = outer.next()
            val g = groups.updated(k, groups.getOrElse(k,0)+v)
            groups = g
            new Distribution[T] { def iterator: Iterator[(T, Int)] = g.iterator } -> 1
          }
        }
      }

    }
  }

  def where(p: T => Boolean): Distribution[T] = {
    factor(t => if(p(t)) 1 else 0)
  }

  def factor(p: T => Int): Distribution[T] = new Distribution[T] {
    def iterator: Iterator[(T, Int)] = {
      val outer = that.iterator
      new Iterator[(T, Int)] {
        def hasNext: Boolean = true
        @tailrec def next(): (T, Int) = {
          val (k, v) = outer.next()
          val w = p(k)
          if(w == 0) next() else (k, v*w)
        }
      }
    }
  }

  def select[S](f: T=>S): Distribution[S] = new Distribution[S] {
    override def iterator: Iterator[(S, Int)] = {
      val outer = that.iterator
      new Iterator[(S,Int)] {
        def hasNext: Boolean = true
        def next(): (S, Int) = {
          val (t, v) = outer.next()
          (f(t), v)
        }
      }
    }
  }

  def then[S](f: T=> Distribution[S]): Distribution[S] = selectMany(f)((x,y) => y)

  def xxx[S,R](f: T=> Distribution[S], c: (T,S) => R): Distribution[R] = ???

  def selectMany[S,R](f: T=> Distribution[S])( c: (T,S) => R): Distribution[R] = new Distribution[R] {
    def iterator: Iterator[(R, Int)] = {
      val outer = that.iterator
      new Iterator[(R,Int)] {
        def hasNext: Boolean = true
        def next(): (R, Int) = {
          val (t,v) = outer.next()
          val (s,w) = f(t).iterator.next()
          (c(t,s), v*w)
        }
      }
    }
  }
}

object Test2 {
  def main(args: Array[String]): Unit = {
    val xs = Uniform(1,2,3,4,5,6).where(x => x != 3)
    val ys = xs.expand()
    println(ys.take(10).toList)
  }
}

object Test {
  def main(args: Array[String]): Unit = {

    val deviation: Distribution[Int] =
      Uniform(Range(start = 5, end = 15): _*)

    println(deviation.sample(1000))

    val prior: Distribution[Int] =
      DiscreteGaussian(10, 2)

    println(prior.sample(100000).toSeq.sortWith(_._2 > _._2).take(5))

    def likelihood(μ: Int): Distribution[Int] = {
      deviation.then(σ => DiscreteGaussian(μ,σ))
    }

    def update(prior: Distribution[Int], iq: Int): Distribution[Int] = {
      prior.selectMany(μ => likelihood(μ).factor(x => {
        val z = (Gaussian.pdf(iq, 0.1, x)*10).asInstanceOf[Int]
        //if(z != 0) println(s"iq = ${iq}, x = ${x}, z = ${z}")
       z
      }))((μ, m) => μ)
    }


    val posterior = update(prior, 8)

    println("...")
    //println(posterior.expand().toSeq.drop(10000).take(1))


    val plot = Vegas("Bob's IQ").
      withXY(posterior.sample(100000).toSeq.take(100)).
      encodeX("x", Quant).
      encodeY("y", Quant).
      mark(Bar)

      plot.show
  }
}

object Graph {
  import vegas._

  def main(args: Array[String]): Unit = {

    val xs = DiscreteGaussian(4, 1)
    //val xs = DiscreteGaussian(4, 1).selectMany(x => DiscreteGaussian(x, 1))((x,y)=>(x,y))
    //  .where({ case (x,y) => y == 3 })
    //  .select({ case (x,y) => x })
    println(xs.sample(1000))

    val plot = Vegas("Country Pop").
      withXY(xs.sample(1000).toSeq).
      //withXY(Bucket(Gaussian(85,10), 1).normalize(100000).toSeq.take(1000)).
      //withXY(DiscreteGaussian(10, 2).normalize(1000).toSeq).
      encodeX("x", Quant).
      encodeY("y", Quant).
      mark(Bar)

    plot.show
  }
}

object MontyHall {
  def main(args: Array[String]): Unit = {

    val doors = Set(1,2,3)

    val k = doors-3

    val xs = Uniform[Int](doors.toSeq: _*).selectMany(prize => Uniform[Int](doors.toSeq: _*))        ((prize, choice)=>(prize,choice))
             .selectMany({ case(prize, choice) => Uniform((doors-prize-choice).toSeq: _*) })         ({ case ((prize,choice), opened) => (prize,choice, opened)})
             .selectMany({ case(prize, choice, opened) => Uniform((doors-choice-opened).toSeq: _*) })({ case ((prize,choice, opened), switch) => (prize,choice, opened, switch)})
             .select({ case (prize,choice, opened, switch) => prize == switch })

    println(xs.sample(1000))

  }
}

object Scratch {

  def main(args: Array[String]): Unit = {
    val xs = DiscreteGaussian(75, 12)
    println(xs.sample(1000))

    val deviation: Distribution[Int] =
      Uniform(Range(start = 5, end = 15): _*)


    /*val ys = xs.factor(x => DiscreteGaussian.pdf(7, 3, x))
    println(ys.normalize(1000))

    val zs = ys.factor(x => DiscreteGaussian.pdf(7, 3, x))
    println(zs.normalize(1000))

    val rs = zs.factor(x => DiscreteGaussian.pdf(7, 3, x))
    println(rs.normalize(1000))

    val ss = rs.factor(x => DiscreteGaussian.pdf(7, 3, x))
    println(ss.normalize(1000))

    val qs = DiscreteGaussian(7, 1)
    println(qs.normalize(1000))*/

    val plot = Vegas("Bob's IQ").
      withXY(xs.sample(100000).toSeq.take(100)).
      encodeX("x", Quant).
      encodeY("y", Quant).
      mark(Bar)

    plot.show

  }
}