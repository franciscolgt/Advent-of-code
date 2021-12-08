package Day5

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day5 extends App {

  // Setup
  val numbersList = Source.fromResource("Day5/input.txt").getLines().toList.map(s => s.split(" -> "))
  val coordsList = getCoordsList(numbersList)

  @tailrec
  def getCoordsList(numbersList: List[Array[String]], coordsList: List[List[(Int, Int)]] = List()): List[List[(Int, Int)]] = {
    if(numbersList.isEmpty) coordsList
    else{
      val start = numbersList.head(0).split(",")
      val end = numbersList.head(1).split(",")
      getCoordsList(numbersList.tail, coordsList :+ List((start(0).toInt, start(1).toInt), (end(0).toInt, end(1).toInt)))
    }
  }

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + numberOfPointsLinesOverlap(coordsList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + numberOfPointsLinesOverlapWithDiagonal(coordsList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def numberOfPointsLinesOverlap(coordsList: List[List[(Int, Int)]], linesOverlap: List[(Int, Int, Int)] = List()): Int = {
    if(coordsList.isEmpty) countNumberLinesOverlap(linesOverlap)
    else{
      val move = coordsList.head
      println(coordsList.size + " Left, Coords: (" + move.head + ", " + move(1) + ")")
      // x is equal
      if(move.head._1.equals(move(1)._1)) {
        val newOverlapList = moveOnX(move.head, move(1), linesOverlap)
        numberOfPointsLinesOverlap(coordsList.tail, newOverlapList)
      }
      // y is equal
      else if(move.head._2.equals(move(1)._2)){
        val newOverlapList = moveOnY(move.head, move(1), linesOverlap)
        numberOfPointsLinesOverlap(coordsList.tail, newOverlapList)
      }
      else numberOfPointsLinesOverlap(coordsList.tail, linesOverlap)
    }
  }

  @tailrec
  def numberOfPointsLinesOverlapWithDiagonal(coordsList: List[List[(Int, Int)]], linesOverlap: List[(Int, Int, Int)] = List()): Int = {
    if(coordsList.isEmpty) countNumberLinesOverlap(linesOverlap)
    else{
      val move = coordsList.head
      println(coordsList.size + " Left, Coords: (" + move.head + ", " + move(1) + ")")
      // x is equal
      if(move.head._1.equals(move(1)._1)) {
        val newOverlapList = moveOnX(move.head, move(1), linesOverlap)
        numberOfPointsLinesOverlapWithDiagonal(coordsList.tail, newOverlapList)
      }
      // y is equal
      else if(move.head._2.equals(move(1)._2)){
        val newOverlapList = moveOnY(move.head, move(1), linesOverlap)
        numberOfPointsLinesOverlapWithDiagonal(coordsList.tail, newOverlapList)
      }
      // diagonal move
      else if((move.head._1 - move(1)._1).abs == (move.head._2 - move(1)._2).abs){
        val newOverlapList = moveOnDiagonal(move.head, move(1), linesOverlap)
        numberOfPointsLinesOverlapWithDiagonal(coordsList.tail, newOverlapList)
      }
      else numberOfPointsLinesOverlapWithDiagonal(coordsList.tail, linesOverlap)
    }
  }

  // Moves on axis
  @tailrec
  def moveOnX(start: (Int, Int), end: (Int, Int), overlapList: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    if(start._2 == end._2) {
      val finalOverlapList = addToList(start, overlapList)
      finalOverlapList
    }
    else {
      val newOverlapList = addToList(end, overlapList)
      if(start._2 > end._2) moveOnX(start, (end._1, end._2+1), newOverlapList)
      else moveOnX(start, (end._1, end._2-1), newOverlapList)
    }
  }

  @tailrec
  def moveOnY(start: (Int, Int), end: (Int, Int), overlapList: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    if(start._1 == end._1) {
      val finalOverlapList = addToList(start, overlapList)
      finalOverlapList
    }
    else {
      val newOverlapList = addToList(end, overlapList)
      if(start._1 > end._1) moveOnY(start, (end._1+1, end._2), newOverlapList)
      else moveOnY(start, (end._1-1, end._2), newOverlapList)
    }
  }

  @tailrec
  def moveOnDiagonal(start: (Int, Int), end: (Int, Int), overlapList: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    if(start == end) addToList(end, overlapList)
    else {
      val newOverlapList = addToList(start, overlapList)
      if(start._1 < end._1 && start._2 < end._2) moveOnDiagonal((start._1 + 1, start._2 + 1), end, newOverlapList)
      else if(start._1 > end._1 && start._2 > end._2) moveOnDiagonal((start._1 - 1, start._2 - 1), end, newOverlapList)
      else if(start._1 > end._1 && start._2 < end._2) moveOnDiagonal((start._1 - 1, start._2 + 1), end, newOverlapList)
      else moveOnDiagonal((start._1 + 1, start._2 - 1), end, newOverlapList)
    }
  }

  // Overlap count on position
  def addToList(point: (Int, Int), overlapList: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    val coordExist = overlapList.filter(c => c._1 == point._1 && c._2 == point._2)
    if(coordExist.isEmpty) overlapList :+ (point._1, point._2, 1)
    else {
      val index = overlapList.indexWhere(p => p._1 == point._1 && p._2 == point._2)
      val updatedPoint = (overlapList(index)._1, overlapList(index)._2, overlapList(index)._3 + 1)
      overlapList.filter(p => p._1 != point._1 || p._2 != point._2) :+ updatedPoint
    }
  }

  // Total overlap count
  @tailrec
  def countNumberLinesOverlap(linesOverlap: List[(Int, Int, Int)], linesOverlapCounter: Int = 0): Int = {
    if(linesOverlap.isEmpty) linesOverlapCounter
    else{
      if(linesOverlap.head._3 > 1) countNumberLinesOverlap(linesOverlap.tail, linesOverlapCounter + 1)
      else countNumberLinesOverlap(linesOverlap.tail, linesOverlapCounter)
    }
  }
}
