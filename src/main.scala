import Probabilities._

import scala.util.Random

object main {

  def main(args: Array[String]): Unit = {

    val xs = uniform("eric","lippert")
    println(xs.take(1000000).groupBy(n => n).map(xn => (xn._1, xn._2.size)))


    val ys = metropolis(xs)(n => if(n == "lippert") 7 else 3)
    println(ys.take(1000000).groupBy(n => n).map(xn => (xn._1, xn._2.size)))

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
    scanl(prior)((o, n) => if(Random.nextDouble() < likelyhood(n) / likelyhood(o)) n else o)
  }

}
