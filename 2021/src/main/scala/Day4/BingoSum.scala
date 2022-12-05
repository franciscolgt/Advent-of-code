package Day4

import scala.annotation.tailrec

object BingoSum {

  @tailrec
  def getCardUnmarkedNumbers(card: List[List[String]], unmarkedNumbers: List[String] = List()): List[String] = {
    if(card.isEmpty) unmarkedNumbers
    else{
      val rowUnmarkedNumbers = getRowUnmarkedNumbers(card.head)
      getCardUnmarkedNumbers(card.tail, unmarkedNumbers ++ rowUnmarkedNumbers)
    }
  }

  @tailrec
  def getRowUnmarkedNumbers(row: List[String], unmarkedNumbers: List[String] = List()): List[String] = {
    if(row.isEmpty) unmarkedNumbers
    else{
      if(row.head.equals("_")) getRowUnmarkedNumbers(row.tail, unmarkedNumbers)
      else getRowUnmarkedNumbers(row.tail, unmarkedNumbers :+ row.head)
    }
  }

  @tailrec
  def getBingoSum(bingoUnmarkedNumbers: List[String], bingoNr: String, bingoSum: Int = 0): Int = {
    if(bingoUnmarkedNumbers.isEmpty) bingoSum * bingoNr.toInt
    else{
      getBingoSum(bingoUnmarkedNumbers.tail, bingoNr, bingoSum + bingoUnmarkedNumbers.head.toInt)
    }
  }

}
