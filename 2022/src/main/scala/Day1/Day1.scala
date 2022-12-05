package Day1

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  val caloriesList =
    Source.fromResource("Day1/input.txt").getLines().toList
  val d1 = System.nanoTime
  println("========== Day 1 ==========");
  println("Ex1: " + top1Calories(caloriesList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + top3Calories(caloriesList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def elfsList(
      caloriesList: List[String],
      totalCaloriesList: List[Int] = List.empty,
      currentCalories: Int = 0
  ): List[Int] = {
    if (caloriesList.isEmpty)
      totalCaloriesList.appended(currentCalories)
    else {
      if (caloriesList.head.isEmpty())
        elfsList(
          caloriesList.tail,
          totalCaloriesList.appended(currentCalories)
        )
      else
        elfsList(
          caloriesList.tail,
          totalCaloriesList,
          currentCalories + caloriesList.head.toInt
        )
    }
  }

  def top1Calories(caloriesList: List[String]): Int = elfsList(
    caloriesList
  ).sorted.reverse.head

  def top3Calories(caloriesList: List[String]): Int = {
    val sortedList = elfsList(
      caloriesList
    ).sorted.reverse
    sortedList(0) + sortedList(1) + sortedList(2)
  }
}
