package Day2

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val movesList = Source.fromResource("Day2/input.txt").getLines().toList
  println("Ex1: " + getPosition(movesList))
  println("Ex2: " + getPositionWithAim(movesList))

  @tailrec
  def getPosition(movesList: List[String], horizontalValue: Int = 0, depthValue: Int = 0): Int = {
    if(movesList.isEmpty) horizontalValue * depthValue
    else{
      val move = movesList.head.split(" ")
      if(move.head.equals("forward")) getPosition(movesList.tail, horizontalValue + move(1).toInt, depthValue)
      else{
        if(move.head.equals("down")) getPosition(movesList.tail, horizontalValue, depthValue + move(1).toInt)
        else getPosition(movesList.tail, horizontalValue, depthValue - move(1).toInt)
      }
    }
  }

  @tailrec
  def getPositionWithAim(movesList: List[String], horizontalValue: Int = 0, depthValue: Int = 0, aimValue: Int = 0): Int = {
    if(movesList.isEmpty) horizontalValue * depthValue
    else{
      val move = movesList.head.split(" ")
      if(move.head.equals("forward")) getPositionWithAim(movesList.tail, horizontalValue + move(1).toInt, depthValue + move(1).toInt* aimValue, aimValue)
      else{
        if(move.head.equals("down")) getPositionWithAim(movesList.tail, horizontalValue, depthValue, aimValue + move(1).toInt)
        else getPositionWithAim(movesList.tail, horizontalValue, depthValue, aimValue - move(1).toInt)
      }
    }
  }
}
