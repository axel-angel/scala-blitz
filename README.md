
# Scala BlitzView

Prototype of BlitzView: Lightweight, non-strict and parallel-efficient views.

 * Guarantee 1 single access per element, O(t) memory and O(tn) computation for transformer operations.
 * Based on Scala Blitz for configurable parallelism.

See the paper at: https://github.com/axel-angel/blitzview-report .
Automatic dependency on ScalaBlitz.

# Compile and REPL:
```shell
$ sbt
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
val v = Array(1,2,3,4).bview
val v2 = v.flatMap{x => Array(x, x+10)}
v.toArray // Array(1, 2, 3, 4)
val v3 = v.flatMap{x => v2.filter{_ % x == 0}}
v3.toArray // Array(1,11,2,12,3,13,4,14,2,12,4,14,12,3,12,4)
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
