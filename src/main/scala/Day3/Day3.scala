package Day3

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day3 extends App{

  val numbersList = Source.fromResource("Day3/input.txt").getLines().toList

  println("Ex1: " + binaryDiagnostic(numbersList))

  @tailrec
  def binaryDiagnostic(numbersList: List[String], countList: List[(Int, Int)] = List()): Int = {
    if (numbersList.isEmpty) {
      val epsilon = getEpsilon(countList)
      val gamma = getGamma(epsilon.toCharArray)
      Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    }
    else {
      val binaryCounterListUpdated = charCounter(numbersList.head.toCharArray, List(), countList)
      binaryDiagnostic(numbersList.tail, binaryCounterListUpdated)
    }
  }

  @tailrec
  def charCounter(charList: Array[Char], countListUpdated: List[(Int, Int)], countList: List[(Int, Int)] = List()): List[(Int, Int)] = {
    if(charList.isEmpty) countListUpdated
    else{
      // If is the first number to be checked
      if(countList.isEmpty)
        if(charList.head.equals('0')) charCounter(charList.tail, countListUpdated :+ (1, 0))
        else charCounter(charList.tail, countListUpdated :+ (0, 1))
      else
        if(charList.head.equals('0')) charCounter(charList.tail, countListUpdated :+ (countList.head._1 + 1, countList.head._2), countList.tail)
      else charCounter(charList.tail, countListUpdated :+ (countList.head._1, countList.head._2 + 1), countList.tail)
    }
  }

  @tailrec
  def getEpsilon(countList: List[(Int, Int)], gamma: String = ""): String = {
    if(countList.isEmpty) gamma
    else{
      if(countList.head._1 >= countList.head._2) getEpsilon(countList.tail, gamma.concat("1"))
      else getEpsilon(countList.tail, gamma.concat("0"))
    }
  }

  @tailrec
  def getGamma(gamma: Array[Char], epsilon: String = ""): String = {
    if(gamma.isEmpty) epsilon
    else{
      if(gamma.head.equals('0')) getGamma(gamma.tail, epsilon.concat("1"))
      else getGamma(gamma.tail, epsilon.concat("0"))
    }
  }

  // ------------------------------------------------------------------------------------------------------------------------------------------------------------

  println("Ex2: " + lifeSupportRating(numbersList))

  def lifeSupportRating(numbersList: List[String]): Int = {
    val oxygenRating = getRating(highest = true, numbersList)
    val co2Rating = getRating(highest = false, numbersList)
    Integer.parseInt(oxygenRating, 2) * Integer.parseInt(co2Rating, 2)
  }

  @tailrec
  def getRating(highest: Boolean, numbersList: List[String], index: Int = 0): String = {
    if(numbersList.size == 1) numbersList.head
    else{
      val bit = getMostCommonBitAtIndex(index, numbersList, highest)
      val validNumbersList = getValidNumbersList(index, bit, numbersList)
      getRating(highest, validNumbersList, index + 1)
    }
  }

  @tailrec
  def getValidNumbersList(index: Int, bit: Char, numbersList: List[String], validNumbersList: List[String] = List()): List[String] = {
    if(numbersList.isEmpty) validNumbersList
    else{
      if(numbersList.head.charAt(index).equals(bit)) getValidNumbersList(index, bit, numbersList.tail, validNumbersList :+ numbersList.head)
      else getValidNumbersList(index, bit, numbersList.tail, validNumbersList)
    }
  }

  @tailrec
  def getMostCommonBitAtIndex(index: Int, numbersList: List[String], highest: Boolean, zeroCount: Int = 0, oneCount: Int = 0): Char = {
    if(numbersList.isEmpty) {
      if(highest)
        if(zeroCount == oneCount) '1'
        else if(oneCount > zeroCount) '1' else '0'
      else
        if(zeroCount == oneCount) '0'
        else if(oneCount > zeroCount) '0' else '1'
    } else{
      if(numbersList.head.charAt(index).equals('1')) getMostCommonBitAtIndex(index, numbersList.tail, highest, zeroCount, oneCount + 1)
      else getMostCommonBitAtIndex(index, numbersList.tail, highest, zeroCount + 1, oneCount)
    }
  }
}
