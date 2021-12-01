package Day1

import scala.io.Source

object Day1 extends App {

  val numbersList = Source.fromResource("Day1/input.txt").getLines().toList.map(s => s.toInt)
  println("Ex1: " + increaseCounter(numbersList))
  println("Ex2: " + increaseCounterOnThreeNumbersSum(numbersList))

  def increaseCounter(numbersList: List[Int], previousNumber: Int = numbersList.head, counter: Int = 0): Int = {
    if (numbersList.isEmpty) counter
    else {
      if (numbersList.head > previousNumber) increaseCounter(numbersList.tail, numbersList.head, counter + 1)
      else increaseCounter(numbersList.tail, numbersList.head, counter)
    }
  }

  def increaseCounterOnThreeNumbersSum(numbersList: List[Int], previousNumberSum: Int = numbersList.head + numbersList(1) + numbersList(2), counter: Int = 0): Int = {
    if (numbersList.size <= 2) counter
    else {
      val numbersSum = numbersList.head + numbersList(1) + numbersList(2)
      if (numbersSum > previousNumberSum) increaseCounterOnThreeNumbersSum(numbersList.tail, numbersSum, counter + 1)
      else increaseCounterOnThreeNumbersSum(numbersList.tail, numbersSum, counter)
    }
  }
}