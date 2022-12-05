package Day4

import scala.annotation.tailrec

object NumberCheck {

  @tailrec
  def checkCards(bingoNr: String, cards: List[List[List[String]]], cardsChecked: List[List[List[String]]] = List()): List[List[List[String]]] = {
    if(cards.isEmpty) cardsChecked
    else{
      val checkedCard = checkSingleCard(bingoNr, cards.head)
      checkCards(bingoNr, cards.tail, cardsChecked :+ checkedCard)
    }
  }

  @tailrec
  def checkSingleCard(bingoNr: String, card: List[List[String]], cardChecked: List[List[String]] = List()): List[List[String]] = {
    if(card.isEmpty) cardChecked
    else{
      val checkedRow = checkSingleRow(bingoNr, card.head)
      checkSingleCard(bingoNr, card.tail, cardChecked :+ checkedRow)
    }
  }

  @tailrec
  def checkSingleRow(bingoNr: String, row: List[String], rowChecked: List[String] = List()): List[String] = {
    if(row.isEmpty) rowChecked
    else{
      if(row.head.equals(bingoNr)) checkSingleRow(bingoNr, row.tail, rowChecked :+ "_")
      else checkSingleRow(bingoNr, row.tail, rowChecked :+ row.head)
    }
  }

  @tailrec
  def checkLastBingoCard(bingo: List[String], cards: List[List[List[String]]]): List[List[String]] = {
    val checkedCard = NumberCheck.checkCards(bingo.head, cards)
    val lastCardHasBingo = BingoVerification.verifyBingo(checkedCard)
    if(lastCardHasBingo.nonEmpty) lastCardHasBingo
    else checkLastBingoCard(bingo.tail, checkedCard)
  }

}
