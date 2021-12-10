package Day10

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day10 extends App {

  // Setup
  val symbolsTextList = Source.fromResource("Day10/input.txt").getLines().toList

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + getCorruptedLinesPoints(symbolsTextList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + getMiddleIncompleteLinesScore(symbolsTextList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def getCorruptedLinesPoints(symbolsTextList: List[String], points: Int = 0): Int = {
    if(symbolsTextList.isEmpty) points
    else{
      val symbolsLineList = symbolsTextList.head.toCharArray.toList
      getCorruptedLinesPoints(symbolsTextList.tail, points + getPointsOfCorruptedLine(symbolsLineList))
    }
  }

  @tailrec
  def getPointsOfCorruptedLine(symbolLineList: List[Char], openedSymbols: List[Char] = List()): Int = {
    if(symbolLineList.isEmpty) 0
    else{
      symbolLineList.head match {
        case '(' => getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols :+ '(')
        case '[' => getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols :+ '[')
        case '{' => getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols :+ '{')
        case '<' => getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols :+ '<')
        case ')' => if(openedSymbols.last == '(') getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols.dropRight(1)) else 3
        case ']' => if(openedSymbols.last == '[') getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols.dropRight(1)) else 57
        case '}' => if(openedSymbols.last == '{') getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols.dropRight(1)) else 1197
        case '>' => if(openedSymbols.last == '<') getPointsOfCorruptedLine(symbolLineList.tail, openedSymbols.dropRight(1)) else 25137
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------------------------

  @tailrec
  def getMiddleIncompleteLinesScore(symbolsTextList: List[String], incompleteLines: List[List[Char]] = List()): Long = {
    if(symbolsTextList.isEmpty) {
      getScoreOfIncompleteLines(incompleteLines)
    }
    else{
      val symbolsLineList: List[Char] = symbolsTextList.head.toCharArray.toList
      val corruptedLineScore = getPointsOfCorruptedLine(symbolsLineList)
      if(corruptedLineScore == 0) getMiddleIncompleteLinesScore(symbolsTextList.tail, incompleteLines :+ getIncompleteLines(symbolsLineList))
      else getMiddleIncompleteLinesScore(symbolsTextList.tail, incompleteLines)
    }
  }

  @tailrec
  def getScoreOfIncompleteLines(incompleteLines: List[List[Char]], scores: List[Long] = List()): Long = {
    if(incompleteLines.isEmpty) {
      val sortedScoresList = scores.sorted
      sortedScoresList(sortedScoresList.size / 2)
    }
    else{
      getScoreOfIncompleteLines(incompleteLines.tail, scores :+ calculateIncompleteLineScore(incompleteLines.head))
    }
  }

  @tailrec
  def getIncompleteLines(symbolLineList: List[Char], openedSymbols: List[Char] = List()): List[Char] = {
    if(symbolLineList.isEmpty) openedSymbols
    else{
      symbolLineList.head match {
        case '(' => getIncompleteLines(symbolLineList.tail, openedSymbols :+ '(')
        case '[' => getIncompleteLines(symbolLineList.tail, openedSymbols :+ '[')
        case '{' => getIncompleteLines(symbolLineList.tail, openedSymbols :+ '{')
        case '<' => getIncompleteLines(symbolLineList.tail, openedSymbols :+ '<')
        case ')' => getIncompleteLines(symbolLineList.tail, openedSymbols.dropRight(1))
        case ']' => getIncompleteLines(symbolLineList.tail, openedSymbols.dropRight(1))
        case '}' => getIncompleteLines(symbolLineList.tail, openedSymbols.dropRight(1))
        case '>' => getIncompleteLines(symbolLineList.tail, openedSymbols.dropRight(1))
      }
    }
  }

  @tailrec
  def calculateIncompleteLineScore(openedSymbolsList: List[Char], score: Long = 0): Long = {
    if(openedSymbolsList.isEmpty) score
    else{
      openedSymbolsList.last match {
        case '(' => calculateIncompleteLineScore(openedSymbolsList.dropRight(1), (score * 5) + 1)
        case '[' => calculateIncompleteLineScore(openedSymbolsList.dropRight(1), (score * 5) + 2)
        case '{' => calculateIncompleteLineScore(openedSymbolsList.dropRight(1), (score * 5) + 3)
        case '<' => calculateIncompleteLineScore(openedSymbolsList.dropRight(1), (score * 5) + 4)
      }
    }
  }
}