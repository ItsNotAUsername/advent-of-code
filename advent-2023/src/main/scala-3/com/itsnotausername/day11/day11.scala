package com.itsnotausername.day11

import com.itsnotausername.files.loadFile

import scala.annotation.tailrec

@main def part1(): Unit =
  val input  = loadFile("day11/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day11/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswerPart1(input: Seq[String]): Long =
  makeAnswer(input, 2)

private def makeAnswerPart2(input: Seq[String]): Long =
  makeAnswer(input, 1000000)

private def makeAnswer(input: Seq[String], expandFactor: Int): Long =
  val universe = parseInput(input)

  val countsByRow = calculateCounts(universe)
  val countsByCol = calculateCounts(universe.transpose)

  calculateAllSum(countsByRow, expandFactor) + calculateAllSum(countsByCol, expandFactor)
end makeAnswer

private def calculateCounts(universe: List[List[Char]]): List[Int] =
  universe.map(_.count(_ == '#'))

// Assume that all points are sorted by coordinate
// Let d(i, j) be the distance between i-th and j-th points
// Let S(i) be the sum of all distances from i-th point to all points before:
//   S(i) = d(i, i - 1) + d(i, i - 2) + ... + d(i, 0)
// Then S(i + 1) = d(i + 1, i) + d(i + 1, i - 1) + ... + d(i + 1, 0)
//               = d(i + 1, i) + (d(i + 1, i) + d(i, i - 1)) + ... + (d(i + 1, i) + d(i + 1, 0))
//               = d(i + 1, i) * i + S(i)
private def calculateAllSum(counts: List[Int], expandFactor: Int) =
  @tailrec
  def go(counts: List[Int])(galaxiesBefore: Int, currentDist: Long, prevSum: Long, acc: Long): Long =
    counts match
      case 0 :: tail =>
        val updatedDist = currentDist + expandFactor
        go(tail)(galaxiesBefore, updatedDist, prevSum, acc)

      case count :: tail =>
        val newSum = currentDist * galaxiesBefore + prevSum
        go(tail)(galaxiesBefore + count, 1, newSum, acc + newSum * count)

      case Nil =>
        acc
  end go

  go(counts)(0, 0, 0, 0)
end calculateAllSum

private def parseInput(input: Seq[String]): List[List[Char]] =
  input.map(_.toList).toList
