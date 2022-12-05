package Day2

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val roundsList =
    Source.fromResource("Day2/input.txt").getLines().toList
  val d1 = System.nanoTime
  println("========== Day 2 ==========");
  println("Ex1: " + strategyScore(roundsList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + correctStrategyScore(roundsList))
  println("Ex2 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def strategyScore(
      roundsList: List[String],
      totalScore: Int = 0
  ): Int = {
    if (roundsList.isEmpty)
      totalScore
    else {
      val plays = roundsList.head.split(" ")
      val opponent = plays.head
      val myPlay = plays(1)

      myPlay match {
        case "X" =>
          opponent match {
            case "A" => strategyScore(roundsList.tail, totalScore + 4)
            case "B" => strategyScore(roundsList.tail, totalScore + 1)
            case "C" => strategyScore(roundsList.tail, totalScore + 7)
          }
        case "Y" =>
          opponent match {
            case "A" => strategyScore(roundsList.tail, totalScore + 8)
            case "B" => strategyScore(roundsList.tail, totalScore + 5)
            case "C" => strategyScore(roundsList.tail, totalScore + 2)
          }
        case "Z" =>
          opponent match {
            case "A" => strategyScore(roundsList.tail, totalScore + 3)
            case "B" => strategyScore(roundsList.tail, totalScore + 9)
            case "C" => strategyScore(roundsList.tail, totalScore + 6)
          }
      }
    }
  }

  @tailrec
  def correctStrategyScore(
      roundsList: List[String],
      totalScore: Int = 0
  ): Int = {
    if (roundsList.isEmpty)
      totalScore
    else {
      val plays = roundsList.head.split(" ")
      val opponent = plays.head
      val myPlay = plays(1)

      myPlay match {
        case "X" =>
          opponent match {
            case "A" => correctStrategyScore(roundsList.tail, totalScore + 3)
            case "B" => correctStrategyScore(roundsList.tail, totalScore + 1)
            case "C" => correctStrategyScore(roundsList.tail, totalScore + 2)
          }
        case "Y" =>
          opponent match {
            case "A" => correctStrategyScore(roundsList.tail, totalScore + 4)
            case "B" => correctStrategyScore(roundsList.tail, totalScore + 5)
            case "C" => correctStrategyScore(roundsList.tail, totalScore + 6)
          }
        case "Z" =>
          opponent match {
            case "A" => correctStrategyScore(roundsList.tail, totalScore + 8)
            case "B" => correctStrategyScore(roundsList.tail, totalScore + 9)
            case "C" => correctStrategyScore(roundsList.tail, totalScore + 7)
          }
      }
    }
  }
}
