package euler.cubedigitpairs

/**
 * https://projecteuler.net/problem=90
 *
 * @author Patrick Sy (patrick.sy@get-it.us)
 */
object CubeDigitPairs extends App {

  case class Cube(digits: Set[Int])

  def canDisplayAll(cubes: Tuple2[Cube, Cube], squares: Seq[Int]): Boolean = {

    def canDisplay(x: Int): Boolean = {
      true
    }

    squares.forall(canDisplay)
  }

  val squares = for (x <- 1 to 9) yield x * x




}
