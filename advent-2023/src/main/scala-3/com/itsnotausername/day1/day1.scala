package com.itsnotausername.day1

import com.itsnotausername.files.loadFile

@main def part1(): Unit =
  val input  = loadFile("day1/input.txt")
  val answer = makeAnswer(input, digitsFromRowPart1)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day1/input.txt")
  val answer = makeAnswer(input, digitsFromRowPart2)

  println(s"The answer is $answer")

private def makeAnswer(input: Seq[String], digitsFromRow: String => (Int, Int)): Int =
  input.foldLeft(0) { case (sum, row) =>
    val (firstDigit, lastDigit) = digitsFromRow(row)
    val number = firstDigit * 10 + lastDigit

    sum + number
  }

private def digitsFromRowPart1(row: String): (Int, Int) =
  row.foldLeft((-1, -1)) {
    case (digits @ (fst, _), currentChar) =>
      if currentChar.isDigit then
        val digit = currentChar.asDigit
        if fst == -1 then (digit, digit) else (fst, digit)
      else
        digits
  }

private def digitsFromRowPart2(row: String): (Int, Int) =
  val str2digit = Map(
    // words
    "one"   -> 1,
    "two"   -> 2,
    "three" -> 3,
    "four"  -> 4,
    "five"  -> 5,
    "six"   -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine"  -> 9,
    // digits
    "1"     -> 1,
    "2"     -> 2,
    "3"     -> 3,
    "4"     -> 4,
    "5"     -> 5,
    "6"     -> 6,
    "7"     -> 7,
    "8"     -> 8,
    "9"     -> 9,
  )

  // (?=(one|two|...|nine|0|1|...|9))
  val regex = str2digit
    .keys
    .mkString("(?=(", "|", "))")
    .r

  val matches = regex
    .findAllMatchIn(row)
    .map(mch => str2digit(mch.group(1)))

  matches.foldLeft((-1, -1)) {
    case ((fst, _), digit) =>
      if fst == -1 then (digit, digit) else (fst, digit)
  }
end digitsFromRowPart2
