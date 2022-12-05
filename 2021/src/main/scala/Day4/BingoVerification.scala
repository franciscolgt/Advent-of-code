package Day4

import scala.annotation.tailrec

object BingoVerification {

  @tailrec
  def verifyBingo(cards: List[List[List[String]]]): List[List[String]] = {
    if(cards.isEmpty) List()
    else{
      val cardBingo = verifyCard(cards.head, cards.head)
      if(cardBingo) cards.head
      else verifyBingo(cards.tail)
    }
  }

  @tailrec
  def verifyLastBingo(cards: List[List[List[String]]], cardsWithoutBingo: List[List[List[String]]] = List()): List[List[List[String]]] = {
    if(cards.isEmpty) cardsWithoutBingo
    else{
      val cardBingo = verifyCard(cards.head, cards.head)
      if(cardBingo) verifyLastBingo(cards.tail, cardsWithoutBingo)
      else verifyLastBingo(cards.tail, cardsWithoutBingo :+ cards.head)
    }
  }

  @tailrec
  def verifyCard(card: List[List[String]], cardTemp: List[List[String]]): Boolean = {
    if(cardTemp.isEmpty) false
    else{
      val rowBingo = verifyRow(cardTemp.head)
      val columnBingo = verifyCardColumns(card)
      if(rowBingo || columnBingo) true
      else verifyCard(card, cardTemp.tail)
    }
  }

  @tailrec
  def verifyRow(row: List[String]): Boolean = {
    if(row.isEmpty) true
    else{
      if(row.head.equals("_")) verifyRow(row.tail)
      else false
    }
  }

  @tailrec
  def verifyCardColumns(card: List[List[String]], index: Int = 0): Boolean = {
    if(card.isEmpty) false
    else{
      if(verifyColumn(card, index)) true
      else if(index == 4) false
      else verifyCardColumns(card, index + 1)
    }
  }

  @tailrec
  def verifyColumn(card: List[List[String]], index: Int): Boolean = {
    if(card.isEmpty) true
    else{
      if(card.head(index).equals("_")) verifyColumn(card.tail, index)
      else false
    }
  }

}
