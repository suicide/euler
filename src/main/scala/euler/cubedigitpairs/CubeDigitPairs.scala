package euler.cubedigitpairs

/**
 * https://projecteuler.net/problem=90
 *
 * @author Patrick Sy (patrick.sy@get-it.us)
 */
object CubeDigitPairs extends App {

  case class Dice(digits: Set[Digit])

  case class Digit(value: Int) {
    def canDisplay(digit: Digit): Boolean = {

      def sixNine = digit.value.equals(6) || digit.value.equals(9)
      value match {
        case 6 => sixNine
        case 9 => sixNine
        case _ => digit.value.equals(value)
      }
    }
  }

  def generateDices(faces: Int, digits: Seq[Int]): Set[Dice] = {

    def generateDice(dice: Set[Int], digits: Set[Int]): Set[Dice] = {
      if (dice.size.equals(faces)) {
        Set(Dice(dice.map(Digit(_))))
      } else {
        val cubes = for {
          x <- digits
        } yield {
          generateDice(dice + x, digits - x)
        }

        cubes.flatten
      }
    }

    generateDice(Set(), digits.toSet)

  }

  def canDisplay(number: Int, dices: Tuple2[Dice, Dice]): Boolean = {
    val firstDigit = Digit(number / 10)
    val secondDigit = Digit(number % 10)

    dices._1.digits.exists(_.canDisplay(firstDigit)) && dices._2.digits.exists(_.canDisplay(secondDigit))
  }

  val squares = for (x <- 1 to 9) yield x * x

  val dices = generateDices(6, 0 to 9)

  val validDices = for {
    n <- squares
    d1 <- dices
    d2 <- dices
    if canDisplay(n, (d1, d2))
  } yield (d1, d2)


  println(validDices.toSet.size)

  // quick hack
  def expandDiceValue(dice: Dice): Dice = {
    val six = Digit(6)
    val nine = Digit(9)

    if (dice.digits.contains(six)) {
      return Dice(dice.digits + nine)
    } else if (dice.digits.contains(nine)) {
      return Dice(dice.digits + six)
    }

    dice

  }

  println(validDices.map( d => {
    (expandDiceValue(d._1), expandDiceValue(d._2))
  }).toSet.size)
}
