import scala.math._

object Main extends App {
  (0 to 100)
    .map(BigInt(2).pow(_))
    .map(x => {
      println(s"Hello ${x}")
    })
}
