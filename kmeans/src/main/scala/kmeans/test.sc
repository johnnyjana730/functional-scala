val pairs = Set((1, "one"),
  (1, "uno"),
  (1, "a"),
  (2, "two"),
  (2, "dos"),
  (2, "b"))

val tmp = pairs.groupBy(_._1)
println(tmp)
//
//
val tmp = pairs.groupBy(_._1).transform((key, value) => ( value.map(_._2)))


//(case(k -> v) => (k -> v.map(_._2)))
println(tmp)
//
//
//val tmp = pairs.groupBy(_._1).map(case(k,v) => (k, v.map(_._2)))
//println(tmp)