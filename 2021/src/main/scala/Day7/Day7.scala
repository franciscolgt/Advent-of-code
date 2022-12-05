package Day7

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day7 extends App {

  // Setup
  val numbersString = Source.fromResource("Day7/input.txt").getLines().toList.head
  val numbersList = numbersString.split(",").toList.map(s => s.toInt)

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + getFuelSpentOnAlignment(numbersList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + getFuelSpentOnAlignmentWithIncrease(numbersList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  def getFuelSpentOnAlignment(numbersList: List[Int]): Int ={
    val median = getMedian(numbersList)
    val fuelSpent = calculateFuelSpent(numbersList,median)
    fuelSpent
  }

  def getFuelSpentOnAlignmentWithIncrease(numbersList: List[Int]): Int ={
    val average = (numbersList.sum.toFloat / numbersList.length.toFloat).toInt
    val fuelSpent = calculateFuelSpentWithIncrease(numbersList,average)
    fuelSpent
  }

  // Calculate total fuel
  @tailrec
  def calculateFuelSpent(numbersList: List[Int], median: Int, totalFuel: Int = 0): Int = {
    if(numbersList.isEmpty) totalFuel
    else calculateFuelSpent(numbersList.tail, median, totalFuel + (numbersList.head - median).abs)
  }

  @tailrec
  def calculateFuelSpentWithIncrease(numbersList: List[Int], average: Int, totalFuel: Int = 0): Int = {
    if(numbersList.isEmpty) totalFuel
    else calculateFuelSpentWithIncrease(numbersList.tail, average, totalFuel + calculateFuelForCrab((numbersList.head - average).abs))
  }

  // Helper methods
  def calculateFuelForCrab(n: Int): Int = {
    if (n == 0) 0
    else n + calculateFuelForCrab(n-1)
  }

  def getMedian(numbersList: Seq[Int]): Int  = {
    val (lower, upper) = numbersList.sortWith(_<_).splitAt(numbersList.size / 2)
    if (numbersList.size % 2 == 0) (lower.last + upper.head) / 2.0.toInt else upper.head
  }
}