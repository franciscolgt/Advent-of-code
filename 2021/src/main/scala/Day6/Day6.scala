package Day6

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day6 extends App {

  // Setup
  val numbersString = Source.fromResource("Day6/input.txt").getLines().toList.head
  val numbersList = getNumbersListCount(numbersString)

  def getNumbersListCount(numbersString: String): List[Long] = {
    val numbersList = numbersString.split(",").toList.map(s => s.toInt)
    List(
      numbersList.count(nr => nr == 0).toLong,
      numbersList.count(nr => nr == 1).toLong,
      numbersList.count(nr => nr == 2).toLong,
      numbersList.count(nr => nr == 3).toLong,
      numbersList.count(nr => nr == 4).toLong,
      numbersList.count(nr => nr == 5).toLong,
      numbersList.count(nr => nr == 6).toLong,
      numbersList.count(nr => nr == 7).toLong,
      numbersList.count(nr => nr == 8).toLong,
    )
  }

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + getLanternFishCount(numbersList, 80))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + getLanternFishCount(numbersList, 256))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def getLanternFishCount(numbersCountList: List[Long], limitDay: Int, day: Int = 0): Long = {
    if(day == limitDay) numbersCountList.sum
    else{
      val newList = List(
        numbersCountList(1),
        numbersCountList(2),
        numbersCountList(3),
        numbersCountList(4),
        numbersCountList(5),
        numbersCountList(6),
        numbersCountList.head + numbersCountList(7),
        numbersCountList(8),
        numbersCountList.head,
      )
      getLanternFishCount(newList, limitDay, day + 1)
    }
  }
}

