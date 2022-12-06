package Day1

import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {

  val sectionsList =
    Source.fromResource("Day4/input.txt").getLines().toList
  val d1 = System.nanoTime
  println("========== Day 4 ==========");
  println("Ex1: " + overrideSectionsTotal(sectionsList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + overlapSectionsTotal(sectionsList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def overrideSectionsTotal(
      sectionsList: List[String],
      overrideTotal: Int = 0
  ): Int = {
    if (sectionsList.isEmpty)
      overrideTotal
    else {
      val pair = sectionsList.head.split(",")
      val elf1 = pair.head.split("-")
      val elf2 = pair(1).split("-")

      val start1 = elf1.head.toInt
      val start2 = elf2.head.toInt
      val end1 = elf1(1).toInt
      val end2 = elf2(1).toInt

      if (
        (start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1)
      )
        overrideSectionsTotal(sectionsList.tail, overrideTotal + 1)
      else overrideSectionsTotal(sectionsList.tail, overrideTotal)
    }
  }

  def overlapSectionsTotal(
      sectionsList: List[String],
      overlapTotal: Int = 0
  ): Int = {
    if (sectionsList.isEmpty)
      overlapTotal
    else {
      val pair = sectionsList.head.split(",")
      val elf1 = pair.head.split("-")
      val elf2 = pair(1).split("-")

      val start1 = elf1.head.toInt
      val start2 = elf2.head.toInt
      val end1 = elf1(1).toInt
      val end2 = elf2(1).toInt

      if (
        (start1 < start2 && end1 < start2 || start2 < start1 && end2 < start1)
      )
        overlapSectionsTotal(sectionsList.tail, overlapTotal)
      else overlapSectionsTotal(sectionsList.tail, overlapTotal + 1)

    }
  }
}
