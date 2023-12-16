package com.itsnotausername.day12

import com.itsnotausername.files.loadFile

import scala.annotation.experimental
import scala.languageFeature.experimental.*
import scala.util.TupledFunction

@experimental
@main def part1(): Unit =
  val input  = loadFile("day12/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@experimental
@main def part2(): Unit =
  val input  = loadFile("day12/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

@experimental
private def makeAnswerPart1(input: Seq[String]): Long =
  val rows = parseInput(input)

  makeAnswer(rows)
end makeAnswerPart1

@experimental
private def makeAnswerPart2(input: Seq[String]): Long =
  val rows = parseInput(input)

  val updatedRows = rows
    .map { case (springs, groups) =>
      val updatedSprings = (0 until 5).toList.flatMap(_ => '?' :: springs).tail
      val updatedGroups  = (0 until 5).toList.flatMap(_ => groups)

      (updatedSprings, updatedGroups)
    }

  makeAnswer(updatedRows)
end makeAnswerPart2

@experimental
private def makeAnswer(rows: Seq[(List[Char], List[Int])]): Long =
  rows.foldLeft(0L) { case (sum, (springs, groups)) => sum + calculate(springs, groups) }

@experimental
private def calculate(springs: List[Char], groups: List[Int]): Long =
  lazy val go: (List[Char], List[Int], Int) => Long =
    memoize { (springs, groups, currentGroup) =>
      (springs, groups) match
        case ('#' :: restS, group :: _) =>
          if currentGroup < group then go(restS, groups, currentGroup + 1) else 0

        case ('.' :: restS, group :: restG) =>
          if currentGroup == group then
            go(restS, restG, 0)
          else if currentGroup == 0 then
            go(restS, groups, 0)
          else
            0

        case ('?' :: restS, groups) =>
          go('#' :: restS, groups, currentGroup) + go('.' :: restS, groups, currentGroup)

        case (Nil, _) =>
          if groups.isEmpty || (groups.length == 1 && groups.head == currentGroup) then 1 else 0

        case (_, Nil) =>
          if springs.contains('#') then 0 else 1

        case _ => sys.error("Impossible")
    }

  go(springs, groups, 0)
end calculate

// Memoization of every function regardless of its arity
@experimental def memoize[F, Args <: Tuple, R](f: F)(using tf: TupledFunction[F, Args => R]): F =
  val cache = collection.mutable.Map.empty[Args, R]

  val tupledF = tf.tupled(f)

  tf.untupled(args => cache.getOrElseUpdate(args, tupledF(args)))
end memoize

private def parseInput(input: Seq[String]): Seq[(List[Char], List[Int])] =
  def parseRow(row: String): (List[Char], List[Int]) =
    val Array(springs, rawGroups) = row.split("\\s+")

    val groups = rawGroups
      .split(",")
      .map(_.toInt)

    (springs.toList, groups.toList)
  end parseRow

  input.map(parseRow)
end parseInput
