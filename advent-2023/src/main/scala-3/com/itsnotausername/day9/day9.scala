package com.itsnotausername.day9

import com.itsnotausername.files.loadFile

import scala.annotation.tailrec

@main def part1(): Unit =
  val input  = loadFile("day9/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day9/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswerPart1(input: Seq[String]): Long =
  def predictNextValue(seq: List[Long]): Long =
    @tailrec
    def go(seq: List[Long], acc: Long): Long =
      val newSeq = seq.zip(seq.tail).map((a, b) => b - a)

      if newSeq.forall(_ == 0) then
        acc
      else
        go(newSeq, acc + newSeq.last)
    end go

    go(seq, seq.last)
  end predictNextValue

  val sequences = parseInput(input)

  sequences.foldLeft(0L)(_ + predictNextValue(_))
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): BigInt =
  def predictPreviousValue(seq: List[Long]): Long =
    @tailrec
    def go(seq: List[Long], mul: Long, acc: Long): Long =
      val newSeq = seq.zip(seq.tail).map((a, b) => b - a)

      if newSeq.forall(_ == 0) then
        acc
      else
        go(newSeq, mul * -1, acc + mul * newSeq.head)
    end go

    go(seq, -1, seq.head)
  end predictPreviousValue
  
  val sequences = parseInput(input)

  sequences.foldLeft(0L)(_ + predictPreviousValue(_))
end makeAnswerPart2

private def parseInput(input: Seq[String]): Seq[List[Long]] =
  input.map(_.split("\\s+").map(_.toLong).toList)
