package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def countParen(charsArray: Array[Char], count: Int): Boolean = {
      (charsArray, count) match {
        case (_, y) if y < 0 => false
        case (Array(), 0) => true
        case (Array(), _) => false
        case (x, _) => {
          x match {
            case Array('(', _*) => countParen(x.drop(1), count + 1)
            case Array(')', _*) => countParen(x.drop(1), count - 1)
            case _ => countParen(x.drop(1), count)
          }
        }
      }
    }


    chars match {
      case Array() => true
      case x => countParen(x, 0)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, illegalClosingParenthesis: Int, openParenthesis: Int): (Int, Int) = {
      if (idx == until) {
        (illegalClosingParenthesis, openParenthesis)
      }
      else {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, illegalClosingParenthesis, openParenthesis + 1)
          case ')' => if (openParenthesis > 0) {
            traverse(idx + 1, until, illegalClosingParenthesis, openParenthesis - 1)
          }
          else {
            traverse(idx + 1, until, illegalClosingParenthesis + 1, openParenthesis)
          }
          case '_' => traverse(idx + 1, until, illegalClosingParenthesis, openParenthesis)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      def combine(start: (Int, Int), end: (Int, Int)): (Int, Int) = {
        (start._1, end._2 + (start._2 - end._1))
      }

      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val middle = (from + until) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        combine(left, right)
      }
    }


    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
