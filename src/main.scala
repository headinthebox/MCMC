import Probabilities._

import scala.util.Random

object main {

  def main(args: Array[String]): Unit = {

    val xs = uniform(0,1,1,1)
    //println(xs.take(1000000).groupBy(n => n).map(xn => (xn._1, xn._2.size)))

    val zs = uniform((0,1), (1,2), (1,1)).take(100000)
    println(zs.groupBy(xp=>xp._1).map(xn => (xn._1, xn._2.map(xn=> xn._2).sum)))

    val ws = metropolis2(zs)(x => if(x==0) 3 else 1)
    println(ws.groupBy(xp=>xp._1).map(xn => (xn._1, xn._2.map(xn=> xn._2).sum)))

    val ys = metropolis(xs)(n => 1)
    //println(ys.take(1000000).groupBy(n => n).map(xn => (xn._1, xn._2.size)))
    // incremental groupby with Dirichlet process/distribution until confidence close enough
    // (normalization)
    // (x,p) --> add p to alpa for x
  }
}

object Probabilities {

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
    scanl(prior)((o, n) => if(Random.nextDouble() <= likelyhood(n) / likelyhood(o)) n else o) // <?
  }

  def metropolis2[A](prior: Iterable[(A, Int)])(likelyhood: A=>Int): Iterable[(A,Int)] = {
    prior.map(xn => (xn._1, likelyhood(xn._1)*xn._2))
  }
  // http://www.ics.uci.edu/~johnsong/papers/Chib%20and%20Greenberg%20-%20Understanding%20the%20Metropolis-Hastings%20Algorithm.pdf
}
