package Day4

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day4 extends App {

  val source = Source.fromResource("Day4/input.txt").getLines().toList
  val bingo = source.head.split(",").toList
  val cards = getCards(source.tail.tail)

  println("Ex1: " + getBingo(bingo, cards))
  println("Ex2: " + getLastBingo(bingo, cards))

  @tailrec
  def getCards(numbersList: List[String], cardsList: List[List[List[String]]] = List(), card: List[List[String]] = List()): List[List[List[String]]] = {
    if(numbersList.isEmpty) cardsList
    else {
      val line = numbersList.head.split(" ").toList.filter(nr => nr.nonEmpty)
      if(card.size == 5) getCards(numbersList.tail, cardsList :+ card)
      else getCards(numbersList.tail, cardsList, card :+ line)
    }
  }

  @tailrec
  def getBingo(bingo: List[String], cards: List[List[List[String]]]): Int = {
    val checkedCards = NumberCheck.checkCards(bingo.head, cards)
    val bingoVerified = BingoVerification.verifyBingo(checkedCards)
    if(bingoVerified.isEmpty) getBingo(bingo.tail, checkedCards)
    else{
      val cardUnmarkedNumbers = BingoSum.getCardUnmarkedNumbers(bingoVerified)
      BingoSum.getBingoSum(cardUnmarkedNumbers, bingo.head)
    }
  }

  @tailrec
  def getLastBingo(bingo: List[String], cards: List[List[List[String]]]): Int = {
    val checkedCards = NumberCheck.checkCards(bingo.head, cards)
    val lastBingoVerified = BingoVerification.verifyLastBingo(checkedCards)
    if(lastBingoVerified.nonEmpty) getLastBingo(bingo.tail, lastBingoVerified)
    else {
      val lastBingoCard = checkLastBingoCard(bingo, cards)
      val cardUnmarkedNumbers = BingoSum.getCardUnmarkedNumbers(lastBingoCard)
      BingoSum.getBingoSum(cardUnmarkedNumbers, bingo.head)
    }
  }

  @tailrec
  def checkLastBingoCard(bingo: List[String], cards: List[List[List[String]]]): List[List[String]] = {
    val checkedCard = NumberCheck.checkCards(bingo.head, cards)
    val lastCardHasBingo = BingoVerification.verifyBingo(checkedCard)
    if(lastCardHasBingo.nonEmpty) lastCardHasBingo
    else checkLastBingoCard(bingo.tail, checkedCard)

  }

  // ------------------------------------------------------------------------------------------------------------------------------------------------------------

}
