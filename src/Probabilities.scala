import scala.util.Random

import vegas._
import vegas.render.WindowRenderer._

object Probabilities {

  def main(args: Array[String]): Unit = {

    val N = 1000000

    val rr = Stream.continually{ Math.random() <= 1/2.0 }
    println(rr.take(N).groupBy(n => n).map(xn => (xn._1, xn._2.size/N.toDouble)))

    val xs = uniform(0,1)
    println(xs.take(N).groupBy(n => n).map(xn => (xn._1, xn._2.size/N.toDouble)))

    val ys = metropolis(xs)(n => if(n==0) 2 else 1)
    println(ys.take(N).groupBy(n => n).map(xn => (xn._1, xn._2.size/N.toDouble)))
    val zs = uniform((0,1), (1,2), (1,1)).take(100000)
    //println(zs.groupBy(xp=>xp._1).map(xn => (xn._1, xn._2.map(xn=> xn._2).sum)))

    val ws = metropolis2(zs)(x => if(x==0) 3 else 1)
    println(ws.groupBy(xp=>xp._1).map(xn => (xn._1, xn._2.map(xn=> xn._2).sum)))

    //val ys = metropolis(xs)(n => 1)
    //println(ys.take(1000000).groupBy(n => n).map(xn => (xn._1, xn._2.size)))
    // incremental groupby with Dirichlet process/distribution until confidence close enough
    // (normalization)
    // (x,p) --> add p to alpa for x
  }

  def repeat[A](a: A): Iterable[A] = {
    new Iterable[A] {
      override def iterator: Iterator[A] =
        new Iterator[A] {
          override def hasNext: Boolean = true
          override def next(): A = a
        }
    }
  }

  def uniform[A](values: A*): Iterable[A] = {
    new Iterable[A] {
      override def iterator: Iterator[A] = new Iterator[A] {
        val random = new Random()
        override def hasNext: Boolean = true
        override def next(): A = values(random.nextInt(values.length))
      }
    }
  }

  def scanl[A](xs: Iterable[A])(combine: (A,A)=> A): Iterable[A] = {
    new Iterable[A] {
      override def iterator: Iterator[A] = {
        new Iterator[A] {

          val values: Iterator[A] = xs.iterator
          var seed: Option[A] = None

          override def hasNext: Boolean = values.hasNext
          override def next(): A = {
            seed match {
              case None => {
                val next = values.next()
                seed = Some(next)
                next
              }
              case Some(current) => {
                val next = combine(current, values.next())
                seed = Some(next)
                next
              }
            }
          }
        }
      }
    }
  }

  def metropolis[A](prior: Iterable[A])(likelyhood: A=>Double): Iterable[A] = {
    scanl(prior)((o, n) => if(Math.random() <= likelyhood(n) / likelyhood(o)) n else o) // <?
  }

  def metropolis2[A](prior: Iterable[(A, Int)])(likelyhood: A=>Int): Iterable[(A,Int)] = {
    prior.map(xn => (xn._1, likelyhood(xn._1)*xn._2))
  }

  def fac(n: Int): Int = Range(1,n+1).product

  def Beta(alpha: Int, beta: Int)(x: Double) =
    Math.pow(x, alpha-1)* Math.pow(1-x, beta-1)/
      (fac(alpha)*fac(beta)/fac(alpha+beta))

  // http://www.ics.uci.edu/~johnsong/papers/Chib%20and%20Greenberg%20-%20Understanding%20the%20Metropolis-Hastings%20Algorithm.pdf
}

/*
val plot = Vegas("Country Pop").
      withData(
        Seq(
          Map("country" -> "USA", "population" -> 314),
          Map("country" -> "UK", "population" -> 64),
          Map("country" -> "DK", "population" -> 80)
        )
      ).
      encodeX("country", Nom).
      encodeY("population", Quant).
      mark(Bar)

    plot.show

 */