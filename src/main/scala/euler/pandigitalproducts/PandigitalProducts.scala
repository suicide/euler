package euler.pandigitalproducts

/**
 * https://projecteuler.net/problem=32
 *
 * @author Patrick Sy (patrick.sy@get-it.us)
 */
object PandigitalProducts extends App {

  def isPandigitalProduct(x: Int, y: Int): Boolean = {

    def digits(number: Int): List[Int] = {
      if (number < 10) {
        List(number)
      } else {
        number % 10 :: digits(number / 10)
      }
    }

    def isPandigital(digits: Seq[Int]): Boolean = digits.size.equals(9) && digits.toSet.size.equals(9) && !digits.contains(0)

    // duh
    if (x.equals(y)) {
      return false
    }

    val product = x * y

    isPandigital( digits(x) ++ digits(y) ++ digits(product))
  }

  val products = for {
    x <- 1 to 1000
    y <- 1 to 10000
    if isPandigitalProduct(x, y)
  } yield x.toLong * y.toLong

  println(products.toSet.sum)

}
