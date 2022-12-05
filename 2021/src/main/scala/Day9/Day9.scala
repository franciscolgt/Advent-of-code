package Day9

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day9 extends App {

  // Setup
  val inputStrings = Source.fromResource("Day9/input.txt").getLines().toList
  val numbersList = inputStrings.map(line => line.toCharArray.map(ch => ch.toInt - 48).toList)

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + countLowPointsTotal(numbersList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + getLargestBasins(numbersList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def countLowPointsTotal(numbersList: List[List[Int]], index: Int = 0, lowPoints: List[Int] = List()): Int = {
    if(index == numbersList.size) lowPoints.sum  + lowPoints.size
    else{
      var lowPointsOnLine: List[Int] = List()
      if(index == 0) lowPointsOnLine = verifyLine(numbersList(index), List(), numbersList(index + 1))
      else if(index == numbersList.size - 1) lowPointsOnLine = verifyLine(numbersList(index), numbersList(index - 1), List())
      else lowPointsOnLine = verifyLine(numbersList(index), numbersList(index - 1), numbersList(index + 1))
      countLowPointsTotal(numbersList, index + 1, lowPoints ::: lowPointsOnLine)
    }
  }

  @tailrec
  def verifyLine(line: List[Int], previousLine: List[Int], nextLine: List[Int], lowPoints: List[Int] = List(), index: Int = 0): List[Int] = {
    if(index == line.size) lowPoints
    else{
      if(line(index) != 9){
        // Top left corner
        if(index == 0 && previousLine.isEmpty){
          if(line(index) < line(index+1) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Top right corner
        else if(index == line.size-1 && previousLine.isEmpty){
          if(line(index) < line(index-1) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Bottom left corner
        else if(index == 0 && nextLine.isEmpty){
          if(line(index) < line(index+1) &&
            line(index) < previousLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Bottom right corner
        else if(index == line.size-1 && nextLine.isEmpty){
          if(line(index) < line(index-1) &&
            line(index) < previousLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Top line
        else if(previousLine.isEmpty){
          if(line(index) < line(index-1) &&
            line(index) < line(index+1) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Bottom line
        else if(nextLine.isEmpty){
          if(line(index) < line(index-1) &&
            line(index) < line(index+1) &&
            line(index) < previousLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Left line
        else if(index == 0){
          if(line(index) < line(index+1) &&
            line(index) < previousLine(index) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Right line
        else if(index == line.size-1){
          if(line(index) < line(index-1) &&
            line(index) < previousLine(index) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }
        // Center
        else {
          if(line(index) < line(index+1) &&
            line(index) < line(index-1) &&
            line(index) < previousLine(index) &&
            line(index) < nextLine(index)) verifyLine(line, previousLine, nextLine, lowPoints :+ line(index), index + 1)
          else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
        }

      }
      else verifyLine(line, previousLine, nextLine, lowPoints, index + 1)
    }
  }

  @tailrec
  def getLargestBasins(numbersList: List[List[Int]], yIndex: Int = 0, basins: List[Int] = List()): Int = {
    if(yIndex == numbersList.size) {
      val orderedBasins = basins.sorted.reverse
      orderedBasins.head * orderedBasins(1) * orderedBasins(2)
    }
    else{
      val basinsOnLine = checkBasinOnLine(numbersList, yIndex)
      getLargestBasins(basinsOnLine._1, yIndex + 1, basins ::: basinsOnLine._2)
    }
  }

  @tailrec
  def checkBasinOnLine(numbersList: List[List[Int]], yIndex: Int, xIndex: Int = 0, basins: List[Int] = List()): (List[List[Int]], List[Int]) = {
    if(xIndex == numbersList.head.size) (numbersList, basins)
    else{
      if(numbersList(yIndex)(xIndex) != 9) {
        val resultOfLine = checkSidesOfNumber(numbersList, yIndex, xIndex)
        checkBasinOnLine(resultOfLine._1, yIndex, xIndex + 1, basins :+ resultOfLine._2)
      }
      else checkBasinOnLine(numbersList, yIndex, xIndex + 1, basins)
    }
  }

  def checkSidesOfNumber(numbersList: List[List[Int]], yIndex: Int, xIndex: Int = 0, count: Int = 1): (List[List[Int]], Int) = {
    if (numbersList(yIndex)(xIndex) == 9) (numbersList, 0)
    else {
      val newLine = numbersList(yIndex).updated(xIndex, 9)
      val updatedList = numbersList.updated(yIndex, newLine)

      val lSize = numbersList.head.size
      val height = numbersList.size
      // Top left corner
      if (yIndex == 0 && xIndex == 0) {
        val right = checkSidesOfNumber(updatedList, yIndex, xIndex + 1, count)
        val bottom = checkSidesOfNumber(right._1, yIndex + 1, xIndex, count)
        (bottom._1, count + right._2 + bottom._2)
      }
      // Top right corner
      else if (yIndex == 0 && xIndex == lSize - 1) {
        val left = checkSidesOfNumber(updatedList, yIndex, xIndex - 1, count)
        val bottom = checkSidesOfNumber(left._1, yIndex + 1, xIndex, count)
        (bottom._1, count + left._2 + bottom._2)
      }
      // Bottom left corner
      else if (yIndex == height - 1 && xIndex == 0) {
        val top = checkSidesOfNumber(updatedList, yIndex - 1, xIndex, count)
        val right = checkSidesOfNumber(top._1, yIndex, xIndex + 1, count)
        (right._1, count + top._2 + right._2)
      }
      // Bottom right corner
      else if (yIndex == height - 1 && xIndex == lSize - 1) {
        val top = checkSidesOfNumber(updatedList, yIndex - 1, xIndex, count)
        val left = checkSidesOfNumber(top._1, yIndex, xIndex - 1, count)
        (left._1, count + top._2 + left._2)
      }
      // Top line
      else if (yIndex == 0) {
        val left = checkSidesOfNumber(updatedList, yIndex, xIndex - 1, count)
        val right = checkSidesOfNumber(left._1, yIndex, xIndex + 1, count)
        val bottom = checkSidesOfNumber(right._1, yIndex + 1, xIndex, count)
        (bottom._1, count + left._2 + right._2 + bottom._2)
      }
      // Bottom line
      else if (yIndex == height - 1) {
        val left = checkSidesOfNumber(updatedList, yIndex, xIndex - 1, count)
        val right = checkSidesOfNumber(left._1, yIndex, xIndex + 1, count)
        val top = checkSidesOfNumber(right._1, yIndex - 1, xIndex, count)
        (top._1, count + left._2 + right._2 + top._2)
      }
      // Left line
      else if (xIndex == 0) {
        val right = checkSidesOfNumber(updatedList, yIndex, xIndex + 1, count)
        val top = checkSidesOfNumber(right._1, yIndex - 1, xIndex, count)
        val bottom = checkSidesOfNumber(top._1, yIndex + 1, xIndex, count)
        (bottom._1, count + right._2 + top._2 + bottom._2)
      }
      // Right line
      else if (xIndex == lSize - 1) {
        val left = checkSidesOfNumber(updatedList, yIndex, xIndex - 1, count)
        val top = checkSidesOfNumber(left._1, yIndex - 1, xIndex, count)
        val bottom = checkSidesOfNumber(top._1, yIndex + 1, xIndex, count)
        (bottom._1, count + left._2 + top._2 + bottom._2)
      }
      // Center
      else {
        val right = checkSidesOfNumber(updatedList, yIndex, xIndex + 1, count)
        val left = checkSidesOfNumber(right._1, yIndex, xIndex - 1, count)
        val bottom = checkSidesOfNumber(left._1, yIndex + 1, xIndex, count)
        val top = checkSidesOfNumber(bottom._1, yIndex - 1, xIndex, count)
        (top._1, count + right._2 + left._2 + top._2 + bottom._2)
      }
    }
  }
}