package com.itsnotausername.day2

import com.itsnotausername.files.loadFile

@main def part1(): Unit =
  val input  = loadFile("day2/input.txt")
  val answer = makeAnswer(input, numberFromGamePart1)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day2/input.txt")
  val answer = makeAnswer(input, numberFromGamePart2)

  println(s"The answer is $answer")
end part2

private def makeAnswer(input: Seq[String], numberFromGame: Game => Long): Long =
  input
    .foldLeft(0L) { case (sum, row) =>
      val game = gameFromRow(row)
      sum + numberFromGame(game)
    }

private def numberFromGamePart1(game: Game): Long =
  val bag = Map(
    "red"   -> 12,
    "green" -> 13,
    "blue"  -> 14
  )

  val check = game.sets.forall { set =>
    set.cubes.forall(cube => cube.count <= bag(cube.colour))
  }

  if check then game.id else 0L
end numberFromGamePart1

private def numberFromGamePart2(game: Game): Long =
  def updateColourNumber(map: Map[String, Long], cube: Cube): Map[String, Long] =
    map.updatedWith(cube.colour) {
      case Some(prevNumber) => Some(prevNumber max cube.count)
      case None             => Some(cube.count)
    }

  val fewestNumbers = game
    .sets
    .foldLeft(Map.empty[String, Long]) { case (map, set) =>
      set
        .cubes
        .foldLeft(map)(updateColourNumber)
    }

  fewestNumbers.values.product
end numberFromGamePart2

final case class Cube(colour: String, count: Long)
final case class Set(cubes: Array[Cube])
final case class Game(id: Long, sets: Array[Set])

private def gameFromRow(row: String): Game =
  def cubeFromStr(str: String): Cube =
    val Array(count, colour) = str.split(" ")
    Cube(colour, count.toLong)

  val Array(gameStr, setsStr) = row.split(": ")

  val gameId = gameStr.drop(5).toInt

  val sets = setsStr
    .split("; ")
    .map { set =>
      val cubesStr = set.split(", ")
      val cubes = cubesStr.map(cubeFromStr)
      Set(cubes)
    }

  Game(gameId, sets)
end gameFromRow
