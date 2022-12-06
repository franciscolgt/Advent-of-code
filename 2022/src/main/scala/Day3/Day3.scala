package Day1

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  val itemsList =
    Source.fromResource("Day3/input.txt").getLines().toList
  val d1 = System.nanoTime
  println("========== Day 3 ==========");
  println("Ex1: " + prioritiesSum(itemsList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + prioritiesSumTrio(itemsList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def prioritiesSum(
      itemsList: List[String],
      priorityTotal: Int = 0
  ): Int = {
    if (itemsList.isEmpty)
      priorityTotal
    else {
      val rucksack = itemsList.head
      val firstContainer = rucksack.substring(0, rucksack.length() / 2)
      val secondContainer =
        rucksack.substring(rucksack.length() / 2, rucksack.length)

      val matchlist =
        firstContainer.toList.filter(x => secondContainer.toList.contains(x))
      if (matchlist.head.isUpper) {
        prioritiesSum(
          itemsList.tail,
          priorityTotal + matchlist.head.toInt - 38
        )
      } else {
        prioritiesSum(itemsList.tail, priorityTotal + matchlist.head.toInt - 96)
      }

    }
  }

  @tailrec
  def prioritiesSumTrio(
      itemsList: List[String],
      priorityTotal: Int = 0
  ): Int = {
    if (itemsList.isEmpty)
      priorityTotal
    else {
      val rucksack1 = itemsList.head.toList
      val rucksack2 = itemsList(1).toList
      val rucksack3 = itemsList(2).toList

      val matchItem =
        rucksack1
          .filter(x => rucksack2.contains(x))
          .filter(y => rucksack3.contains(y))
          .head
      if (matchItem.isUpper) {
        prioritiesSumTrio(
          itemsList.tail.tail.tail,
          priorityTotal + matchItem.toInt - 38
        )
      } else {
        prioritiesSumTrio(
          itemsList.tail.tail.tail,
          priorityTotal + matchItem.toInt - 96
        )
      }

    }
  }
}
