
val radius = 2
val x = 2
val y = 2

val tmp_x = (for {
  x1 <- -radius to radius
  y1 <- -radius to radius
  if ( (x + x1) <= 10 && (y + y1) <= 10)
  if ( 0 <= (x + x1) && 0 <= (y + y1))
} yield ((x + x1), (y + y1)))

val y = tmp_x.foldLeft((0, 0)) { case ((accA, accB), (a, b)) => (accA + a, accB + b) }

println(tmp_x)

println(y)