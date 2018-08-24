package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (y, _) if y == 0 => 1
      case (y, x) if y < 0 || x.isEmpty => 0
      case (y, x :: xs) => countChange(y - x, x :: xs) + countChange(y, xs)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    (money, coins) match {
      case (y, x) if y < 0 || x.isEmpty => 0
      case (y, _) if y == 0 => 1
      case (y, x :: xs) => {
        if (threshold(y, x::xs)) {
          countChange(y, x::xs)
        } else {
          val tasks: (Int, Int) = parallel(parCountChange(y - x, x::xs, threshold), parCountChange(y, xs, threshold))
          tasks._1 + tasks._2
        }
      }
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    def threshold(money: Int, coins: List[Int]) = money <= ((startingMoney * 2) / 3)

    threshold
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    def threshold(money: Int, coins: List[Int]) = money <= ((totalCoins * 2) / 3)

    threshold
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    def threshold(money: Int, coins: List[Int]) = (money * coins.length) <= ((startingMoney * allCoins.length) / 2)

    threshold
  }
}
