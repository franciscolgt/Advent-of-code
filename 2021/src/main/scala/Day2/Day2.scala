package Day2

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val movesList = Source.fromResource("Day2/input.txt").getLines().toList

  val d1 = System.nanoTime
  println("Ex1: " + getPosition(movesList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + getPositionWithAim(movesList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def getPosition(
      movesList: List[String],
      horizontalValue: Int = 0,
      depthValue: Int = 0
  ): Int = {
    if (movesList.isEmpty) horizontalValue * depthValue
    else {
      val move = movesList.head.split(" ")
      move.head match {
        case "forward" =>
          getPosition(
            movesList.tail,
            horizontalValue + move(1).toInt,
            depthValue
          )
        case "down" =>
          getPosition(
            movesList.tail,
            horizontalValue,
            depthValue + move(1).toInt
          )
        case "up" =>
          getPosition(
            movesList.tail,
            horizontalValue,
            depthValue - move(1).toInt
          )
      }
    }
  }

  @tailrec
  def getPositionWithAim(
      movesList: List[String],
      horizontalValue: Int = 0,
      depthValue: Int = 0,
      aimValue: Int = 0
  ): Int = {
    if (movesList.isEmpty) horizontalValue * depthValue
    else {
      val move = movesList.head.split(" ")
      move.head match {
        case "forward" =>
          getPositionWithAim(
            movesList.tail,
            horizontalValue + move(1).toInt,
            depthValue + move(1).toInt * aimValue,
            aimValue
          )
        case "down" =>
          getPositionWithAim(
            movesList.tail,
            horizontalValue,
            depthValue,
            aimValue + move(1).toInt
          )
        case "up" =>
          getPositionWithAim(
            movesList.tail,
            horizontalValue,
            depthValue,
            aimValue - move(1).toInt
          )
      }
    }
  }
}
