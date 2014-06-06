
# Scala BlitzView

Prototype of BlitzView: Lightweight, non-strict and parallel-efficient views (prototype).

 * Guarantee constant time and constant memory for transformer operations.
 * Based on Scala Blitz for configurable parallelism.

See the paper at: https://github.com/axel-angel/blitzview-report .
The code is in the views folder.
Contains ScalaBlitz: http://scala-blitz.github.io/home/documentation/

# Compile and REPL:
```shell
sbt
> project views
> compile
> console
```

# Code example:
```scala
import scala.collection.views.BlitzView
import scala.collection.views.BlitzView._
import scala.collection.views.BlitzViewImpl._
import scala.collection.par.Scheduler.Implicits.sequential
import collection.mutable.HashMap

val m = HashMap((1,2))
val v = m.bview.map{case (x,y) => x+y}
v.toArray // Array(3)
v.sum // 3
m += ((3,4))
v.toArray // Array(7, 3)
v.sum // 10
```

See the tests
