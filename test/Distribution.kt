package k.Probability

import scala.util.Random

import k.Probability.Weather.*
import k.Probability.Sprinkler.*
import k.Probability.Grass.*
import k.Probability.Weight.*
import k.Probability.Food.*
import k.Probability.Probabilistic.condition
import kotlin.coroutines.experimental.*
import k.Probability.Probabilistic.probabilistic
import k.Probability.Probabilistic.sample
import k.Probability.Probabilistic.count

fun mainY(args: Array<String>) {

    val rain =
            Categorical(Rain to 20, Sunny to 80)

    val sprinkler = {
        weather: Weather -> when (weather) {
            Sunny -> Categorical(On to 40, Off to 60)
            Rain  -> Categorical(On to 1,  Off to 99)
        }
    }

    val grass = { w: Weather, s: Sprinkler ->
         when (Pair(w,s)) {
            Pair(Sunny, Off) -> Categorical(Wet to 0, Dry to 100)
            Pair(Rain, Off)  -> Categorical(Wet to 80, Dry to 20)
            Pair(Sunny, On)  -> Categorical(Wet to 90, Dry to 10)
            Pair(Rain, On)   -> Categorical(Wet to 99, Dry to 1)
            else -> throw Exception()
        }
    }

    val predict = { wet: Grass ->
        probabilistic {
            val r = sample(rain)
            val s = sample(sprinkler(r))
            val g = sample(grass(r,s))
            condition(g == wet)
            r
        }
    }

    println(predict(Wet).sample(1000))

}

fun mainw(args: Array<String>) {

    fun flip(p: Int): Distribution<Boolean> =
        Categorical(true to p, false to 10-p)

    fun coin(p: Int): Distribution<Boolean> = probabilistic {
        sample(flip(p))
    }

    // number of flips to get one success
    fun geometric(p: Int): Distribution<Int> = probabilistic {
         var total = 1
         while(sample(coin(5))) { total +=1 }
         count(total)
    }

    println(geometric(5).sample(100))
}

fun mainq(args: Array<String>) {

    val h = probabilistic {
        val doors = mutableSetOf(1,2,3)

        val prize = sample(Uniform(*(doors).toTypedArray()))
        val choice = sample(Uniform(*(doors).toTypedArray()))
        val opened = sample(Uniform(*(doors-prize-choice).toTypedArray()))
        val switched = sample(Uniform(*(doors-choice-opened).toTypedArray()))
        prize == switched
    }

    println(h.sample(50))
}

fun mainz(args: Array<String>)  {

    val cdc =
        Categorical(Obese to 2, Skinny to 3)

    val doctor = { weight: Weight ->
        when(weight) {
            Obese  -> Categorical(Burger to 9, Celery to 1)
            Skinny -> Categorical(Burger to 3, Celery to 7)
        }
    }

    val predict = { food: Food ->
        probabilistic {
            val w = sample(cdc)
            val f = sample(doctor(w))
            condition(f == food)
            w
        }
    }

    println(predict(Burger).sample(10))
    println(predict(Celery).sample(10))
}

fun mainx(args: Array<String>) {

    val rain: Distribution<Weather> =
            Categorical(Rain to 20, Sunny to 80)

    val sprinkler: (Weather) -> Distribution<Sprinkler> = {
        weather: Weather -> when (weather) {
        Sunny -> Categorical(On to 40, Off to 60)
        Rain  -> Categorical(On to 1,  Off to 99)
    }
    }

    val grass: (Pair<Weather, Sprinkler>) -> Distribution<Grass> = {
        ws : Pair<Weather, Sprinkler> -> when (ws) {
        Pair(Sunny, Off) -> Categorical(Wet to 0, Dry to 100)
        Pair(Rain, Off)  -> Categorical(Wet to 80, Dry to 20)
        Pair(Sunny, On)  -> Categorical(Wet to 90, Dry to 10)
        Pair(Rain, On)   -> Categorical(Wet to 99, Dry to 1)
        else -> throw Exception()
    }
    }

    val predict: (Grass) -> Distribution<Weather> = {
        wet: Grass ->
        rain.flatMap(sprinkler, { r,s -> Pair(r,s) })
                .flatMap(grass, { rs, g -> val (r,s) = rs; Triple(r, s, g) })
                .filter({ rsg -> val (_,_,g) = rsg; g == wet })
                .map({ rsg -> val (r,_,_) = rsg; r })
    }

    println(predict(Wet).sample(1000))

}

fun main(args: Array<String>) {

    //println(Uniform(1,2,3).take(10).toList())

    println(Categorical("a" to 1, "b" to 2).sample(10000))

    println(Categorical("a" to 1, "b" to 2).metropolis().sample(10000))
}

fun main4(args: Array<String>) {

    val h = probabilistic {

        val x = sample(Uniform(1,2,3,4,5,6))
        condition(x != 3)
        x
    }
    println(h.sample(10000))
}

object Probabilistic {

    var total: Int = 1

    fun <T> probabilistic (block: suspend () -> T): Distribution<T>  {
        return object:Distribution<T> {
            override fun iterator(): Iterator<Pair<T, Int>> {
                return object: Iterator<Pair<T, Int>> {
                    override fun hasNext(): Boolean = true

                    override tailrec fun next(): Pair<T, Int> {
                        var r: T? = null
                        block.startCoroutine(object: Continuation<T> {
                            override val context: CoroutineContext
                                get() {
                                    return EmptyCoroutineContext
                                }

                            override fun resume(value: T) {
                                r = value
                            }

                            override fun resumeWithException(exception: Throwable) {
                                total = 0
                            }

                        })

                        val x = total
                        total = 1

                        if(x == 0) {
                            return next()
                        } else {
                            return r!! to x
                        }
                    }

                }
            }

        }
    }

    suspend fun <T> sample(dist: Distribution<T>) = suspendCoroutine { continuation: Continuation<T> ->
            val t = dist.first()
            total *= t.second
            continuation.resume(t.first)
    }

    suspend fun condition(p: Boolean): Unit = suspendCoroutine { continuation: Continuation<Unit> ->
        if(!p) { total = 0 }
        continuation.resume(Unit)
    }

    suspend fun <T>count(t: T): T = suspendCoroutine { continuation: Continuation<T> ->
        total = 1
        continuation.resume(t)
    }
}

enum class Weather {Rain, Sunny }
enum class Sprinkler {On, Off }
enum class Grass { Dry, Wet }

enum class Weight { Obese, Skinny }
enum class Food { Burger, Celery }

object Categorical {
    operator fun <T>invoke(vararg ts: Pair<T, Int>): Distribution<T> {
        return object:Distribution<T> {
            val random = Random()
            val n = ts.size
            override fun iterator(): Iterator<Pair<T, Int>> {
                return object : Iterator<Pair<T, Int>> {

                    override fun hasNext(): Boolean = true

                    override fun next(): Pair<T, Int> = ts[random.nextInt(n)]
                }
            }
        }
    }
}

object Uniform {
    operator fun <T>invoke(vararg ts: T): Distribution<T> {
        return Categorical(*(ts.map{ v -> v to 1}.toTypedArray()))
    }
}

interface Distribution<T> : Iterable<Pair<T, Int>> {

    /**
     * Normalize distribution where every value has weight one.
     */
    fun metropolis(): Distribution<T> {

        return object: Distribution<T> {

            val random = Random()

            override fun iterator(): Iterator<Pair<T, Int>> {

                return object: Iterator<Pair<T,Int>> {

                    val outer = this@Distribution.iterator()
                    var previous: Pair<T, Int>? = null

                    override fun hasNext(): Boolean = true

                    override fun next(): Pair<T, Int> {

                        val (t, p) = outer.next()
                        val (o, q) = previous ?: Pair(t,p)

                        if (random.nextDouble() <= p*1.0/q) {
                            previous = Pair(t,p)
                            return Pair(t,1)
                        } else {
                            previous = Pair(o,q)
                            return Pair(o,1)
                        }
                    }
                }
            }
        }
    }

    fun sample(n: Int): Map<T, Double> {
        val kvs: Map<T, Int> = expand().asSequence().drop(n).first().first.sortedByDescending { kv -> kv.second }.toMap()
        val total: Double = kvs.values.sum()*1.0
        return kvs.mapValues { kv -> kv.value / total }
    }

    fun expand(): Distribution<Distribution<T>> {
        return object : Distribution<Distribution<T>> {

            override fun iterator(): Iterator<Pair<Distribution<T>, Int>> {

                return object : Iterator<Pair<Distribution<T>, Int>> {

                    val outer = this@Distribution.iterator()
                    val groups = kotlin.collections.hashMapOf<T, Int>().withDefault { 0 }

                    override fun hasNext(): Boolean = true

                    override fun next(): Pair<Distribution<T>, Int> {
                        val (k,v) = outer.next()
                        groups[k] = groups.getValue(k)+v
                        val inner = groups.toList()

                        return object : Distribution<T> {
                            override fun iterator(): Iterator<Pair<T, Int>> = inner.iterator()
                        } to 1
                    }

                }
            }

        }
    }

    fun filter(p: (T)->Boolean): Distribution<T> {
        return factor({ t -> if(p(t)) 1 else 0 })
    }

    fun factor(p: (T)->Int): Distribution<T> {

        return object: Distribution<T> {

            override fun iterator(): Iterator<Pair<T, Int>> {

                return object: Iterator<Pair<T, Int>> {

                    val outer = this@Distribution.iterator()

                    override fun hasNext(): Boolean = true

                    override tailrec fun next(): Pair<T, Int> {
                        val (k, v) = outer.next()
                        val w = p(k)
                        if(w == 0) return next() else return Pair(k, v*w)
                    }
                }
            }
        }
    }

    fun <S>map(f: (T)->S): Distribution<S> {
        return object: Distribution<S> {

            override fun iterator(): Iterator<Pair<S, Int>> {

                return object : Iterator<Pair<S, Int>> {

                    val outer = this@Distribution.iterator()

                    override fun hasNext(): Boolean = true

                    override fun next(): Pair<S, Int> {
                        val (t, v) = outer.next()
                        return f(t) to v
                    }
                }
            }
        }
    }

    fun <S>flatMap(f: (T)->Distribution<S>): Distribution<S> {
        return flatMap(f, { _, s->s })
    }

    fun <S, R>flatMap(f: (T)->Distribution<S>, c: (T,S)->R): Distribution<R> {
        return object : Distribution<R> {
            override fun iterator(): Iterator<Pair<R, Int>> {

                return object : Iterator<Pair<R, Int>> {

                    val outer = this@Distribution.iterator()

                    override fun hasNext(): Boolean = true

                    override fun next(): Pair<R, Int> {
                        val (t,v) = outer.next()
                        val (s,w) = f(t).iterator().next()
                        return c(t,s) to v*w
                    }

                }
            }
        }
    }
}

