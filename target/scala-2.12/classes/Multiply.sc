import scala.collection.mutable.HashMap

var group = new HashMap[Int, String]()
println(group)
group.update(1, "hello")
println(group)
val g = group.toIterator
group.update(2, "world")
println(group)
println(g.toList)





















