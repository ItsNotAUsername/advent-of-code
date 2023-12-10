package com.itsnotausername.day6

import com.itsnotausername.files.loadFile

@main def part1(): Unit =
  val input  = loadFile("day6/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day6/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswer(races: List[Race]): Long =
  races.foldLeft(1L)(_ * numberOfWins(_))

private def makeAnswerPart1(input: Seq[String]): Long =
  val races = parseInputPart1(input)

  makeAnswer(races)
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Long =
  val races = parseInputPart2(input)

  makeAnswer(races)
end makeAnswerPart2

final case class Race(time: Long, distance: Long)

private def numberOfWins(race: Race): Long =
  val minTime = (1L to race.time)
    .collectFirst { case time if time * (race.time - time) > race.distance => time }

  minTime.fold(0L)(race.time - 2 * _ + 1)
end numberOfWins

private def parseInputPart1(input: Seq[String]): List[Race] =
  val times = input.head match
    case s"Time: $times" => times.trim.split("\\s+").map(_.toLong)

  val distances = input(1) match
    case s"Distance: $distances" => distances.trim.split("\\s+").map(_.toLong)

  times
    .zip(distances)
    .map { case (time, distance) => Race(time, distance) }
    .toList
end parseInputPart1

private def parseInputPart2(input: Seq[String]): List[Race] =
  val time = input.head match
    case s"Time: $time" => time.replaceAll("\\s+", "").toLong

  val distance = input(1) match
    case s"Distance: $distance" => distance.replaceAll("\\s+", "").toLong

  Race(time, distance) :: Nil
end parseInputPart2
