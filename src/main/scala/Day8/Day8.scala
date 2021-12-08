package Day8

import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.io.Source
import scala.language.postfixOps

object Day8 extends App {

  // Setup
  val inputStrings = Source.fromResource("Day8/input.txt").getLines().toList
  val digitsList = inputStrings.map(str => str.split((" \\| ")).toList.map(s => s.split(" ").toList))//.map(s => s.split(" ").toList)

  // Exercises
  val d1 = System.nanoTime
  println("Ex1: " + countSimpleDigits(digitsList))
  println("Ex1 time: " + (System.nanoTime - d1) / 1e6d + " ms")
  val d2 = System.nanoTime
  println("Ex2: " + findAllDigits(digitsList))
  println("Ex1 time: " + (System.nanoTime - d2) / 1e6d + " ms")

  @tailrec
  def countSimpleDigits(digitsList: List[List[List[String]]], total: Int = 0): Int = {
    if(digitsList.isEmpty) total
    else{
      val one = digitsList.head(1).count(digit => digit.length == 2)
      val four = digitsList.head(1).count(digit => digit.length == 4)
      val seven = digitsList.head(1).count(digit => digit.length == 3)
      val eight = digitsList.head(1).count(digit => digit.length == 7)
      countSimpleDigits(digitsList.tail, total + one + four + seven + eight)
    }
  }

  def findAllDigits(digitsList: List[List[List[String]]], total: Int = 0): Int = {
    if(digitsList.isEmpty) total
    else {
      var list = digitsList.head.head
      // Number one
      val one = list.filter(digit => digit.length == 2).head
      list = list.filter(_ != one)
      // Number four
      val four = list.filter(digit => digit.length == 4).head
      list = list.filter(_ != four)
      // Number seven
      val seven = list.filter(digit => digit.length == 3).head
      list = list.filter(_ != seven)
      // Number eight
      val eight = list.filter(digit => digit.length == 7).head
      list = list.filter(_ != eight)
      // Number nine
      val nine = list.filter(digit => digit.length == 6 && digit.toCharArray.toSet.containsAll(four.toCharArray.toSet)).head
      list = list.filter(_ != nine)
      // Number six
      val six = list.filter(digit => digit.length == 6 && !digit.toCharArray.toSet.containsAll(one.toCharArray.toSet)).head
      list = list.filter(_ != six)
      // Number zero
      val zero = list.filter(digit => digit.length == 6).head
      list = list.filter(_ != zero)
      // Number three
      val three = list.filter(digit => digit.toCharArray.toSet.containsAll(one.toCharArray.toSet)).head
      list = list.filter(_ != three)
      // Number five
      val five = list.filter(digit => nine.toCharArray.toSet.containsAll(digit.toCharArray.toSet)).head
      list = list.filter(_ != five)
      // Number two
      val two = list.head
      list = list.filter(_ != two)

      val orderedDigitList = List(zero, one, two, three, four, five, six, seven, eight, nine)

      var stringCode = ""
      stringCode = stringCode + findDigitOfCode(orderedDigitList, digitsList.head.tail.head.head)
      stringCode = stringCode + findDigitOfCode(orderedDigitList, digitsList.head.tail.head(1))
      stringCode = stringCode + findDigitOfCode(orderedDigitList, digitsList.head.tail.head(2))
      stringCode = stringCode + findDigitOfCode(orderedDigitList, digitsList.head.tail.head(3))

      findAllDigits(digitsList.tail, total + stringCode.toInt)
    }
  }

  def findDigitOfCode(digitsList: List[String], digit: String): String = {
    digitsList.zipWithIndex.filter(str => str._1.length == digit.length && digit.toCharArray.toSet.containsAll(str._1.toCharArray.toSet)).head._2.toString
  }

}