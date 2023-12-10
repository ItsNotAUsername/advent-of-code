package com.itsnotausername.day8

import com.itsnotausername.files.loadFile

import scala.annotation.tailrec

@main def part1(): Unit =
  val input  = loadFile("day8/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day8/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswerPart1(input: Seq[String]): Long =
  val (instructions, nodeMap) = parseInput(input)

  countSteps(nodeMap, instructions)("AAA", _ == "ZZZ")
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): BigInt =
  def lcm(a: BigInt, b: BigInt) = a * b / a.gcd(b)

  val (instructions, nodeMap) = parseInput(input)

  val startNodes = nodeMap
    .keysIterator
    .filter(_.last == 'A')
    .toList

  startNodes
    .map { node =>
      val steps = countSteps(nodeMap, instructions)(node, _.last == 'Z')
      BigInt(steps)
    }
    .reduce(lcm)
end makeAnswerPart2

private def countSteps(
  nodeMap: Map[String, Choice],
  instructions: String
)(
  startNode: String,
  check: String => Boolean
): Long =
  val instrIter = Iterator
    .continually(1)
    .flatMap(_ => instructions)

  @tailrec def go(currentNode: String, acc: Long = 1L): Long =
    val instr    = instrIter.next()
    val nextNode = nodeMap(currentNode).next(instr)

    if check(nextNode) then
      acc
    else
      go(nextNode, acc + 1)
  end go

  go(startNode)
end countSteps

final case class Choice(left: String, right: String):
  def next(instr: Char): String =
    if instr == 'L' then left else right
end Choice

private def parseInput(input: Seq[String]): (String, Map[String, Choice]) =
  val nodeMap = input
    .drop(2)
    .map {
      case s"$key = ($leftValue, $rightValue)" => key -> Choice(leftValue, rightValue)
    }
    .toMap

  (input.head, nodeMap)
end parseInput
