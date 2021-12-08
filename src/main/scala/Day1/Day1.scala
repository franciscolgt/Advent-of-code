package Day1

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  val numbersList = Source.fromResource("Day1/input.txt").getLines().toList.map(s => s.toInt)
  val d1 = System.nanoTime
  println("Ex1: " + increaseCounter(numbersList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + increaseCounterOnThreeNumbersSum(numbersList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def increaseCounter(numbersList: List[Int], previousNumber: Int = numbersList.head, counter: Int = 0): Int = {
    if (numbersList.isEmpty) counter
    else {
      if (numbersList.head > previousNumber) increaseCounter(numbersList.tail, numbersList.head, counter + 1)
      else increaseCounter(numbersList.tail, numbersList.head, counter)
    }
  }

  @tailrec
  def increaseCounterOnThreeNumbersSum(numbersList: List[Int], previousNumberSum: Int = numbersList.head + numbersList(1) + numbersList(2), counter: Int = 0): Int = {
    if (numbersList.size <= 2) counter
    else {
      val numbersSum = numbersList.head + numbersList(1) + numbersList(2)
      if (numbersSum > previousNumberSum) increaseCounterOnThreeNumbersSum(numbersList.tail, numbersSum, counter + 1)
      else increaseCounterOnThreeNumbersSum(numbersList.tail, numbersSum, counter)
    }
  }
}
