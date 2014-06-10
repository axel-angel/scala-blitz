
# Scala BlitzView

Prototype of BlitzView: Lightweight, non-strict and parallel-efficient views.

 * Guarantee 1 single access per element, O(t) memory and O(tn) computation for transformer operations.
 * Based on Scala Blitz for configurable parallelism.

See the paper at: https://github.com/axel-angel/blitzview-report .
The code is in the views folder.
Contains ScalaBlitz: http://scala-blitz.github.io/home/documentation/

# Compile and REPL:
```shell
$ sbt
> project views
> compile
> console
```

# Code examples:
Necessary imports:
```scala
import scala.collection.views.BlitzView
import scala.collection.views.BlitzView._
import scala.collection.views.BlitzViewImpl._
import scala.collection.par.Scheduler.Implicits.sequential
```

Some examples:
```scala
val v = (0 to 10).bview
val u = v.map(_ + 10)
```

```scala
val v = Array(0,1,2,3,4).bview
val v2 = v.flatMap{x => Array(x+10)}
v.toArray // Array(0, 1, 2, 3, 4)
val v3 = v.flatMap{_ => v2.filter{_ % 2 == 0}}
v3.toArray // Array(10,12,14,10,12,14,10,12,14,10,12,14,10,12,14)
```

```scala
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
