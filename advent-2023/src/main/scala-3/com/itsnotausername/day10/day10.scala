package com.itsnotausername.day10

import com.itsnotausername.files.loadFile

import scala.annotation.tailrec

@main def part1(): Unit =
  val input  = loadFile("day10/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day10/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

type Point = (Int, Int)
type Field = Vector[Vector[Char]]

private def makeAnswerPart1(input: Seq[String]): Int =
  val field = parseInput(input)

  val path = buildPath(field)

  path.length / 2
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Int =
  def revealS(field: Field, point: Point): Char =
    val (x, y) = point

    val top    = connectedTo(field)(point, (x - 1, y))
    val left   = connectedTo(field)(point, (x, y - 1))
    val right  = connectedTo(field)(point, (x, y + 1))
    val bottom = connectedTo(field)(point, (x + 1, y))

    if top && bottom then
      '|'
    else if top && left then
      'J'
    else if top && right then
      'L'
    else if left && right then
      '-'
    else if left && bottom then
      '7'
    else if right && bottom then
      'F'
    else // impossible case
      '.'
  end revealS

  def countTilesInRow(field: Field, path: Set[Point], rowIdx: Int): Int =
    @tailrec
    def go(colIdx: Int, prevTile: Char = '.', enclosed: Boolean = false, acc: Int = 0): Int =
      if colIdx >= field(rowIdx).length then
        acc
      else
        val point = (rowIdx, colIdx)
        val tile = field(rowIdx)(colIdx)

        val updatedTile =
          if tile == 'S' then
            revealS(field, point)
          else if path.contains(point) then
            tile
          else
            '.'

        updatedTile match
          case '|' =>
            go(colIdx + 1, prevTile, !enclosed, acc)

          case 'J' =>
            val updatedEnclosed = if prevTile == 'F' then !enclosed else enclosed
            go(colIdx + 1, 'J', updatedEnclosed, acc)

          case 'L' =>
            go(colIdx + 1, 'L', enclosed, acc)

          case '-' =>
            go(colIdx + 1, prevTile, enclosed, acc)

          case '7' =>
            val updatedEnclosed = if prevTile == 'L' then !enclosed else enclosed
            go(colIdx + 1, '7', updatedEnclosed, acc)

          case 'F' =>
            go(colIdx + 1, 'F', enclosed, acc)

          case _ =>
            if enclosed then
              go(colIdx + 1, prevTile, enclosed, acc + 1)
            else
              go(colIdx + 1, prevTile, enclosed, acc)
    end go

    go(0)
  end countTilesInRow

  val field = parseInput(input)
  val path  = buildPath(field).toSet

  field
    .indices
    .foldLeft(0) { case (sum, rowIdx) => sum + countTilesInRow(field, path, rowIdx) }
end makeAnswerPart2

@tailrec
def connectedTo(field: Field)(from: Point, to: Point): Boolean =
  val (fromX, fromY) = from
  val (toX, toY) = to

  if toX < 0 || toX >= field.length || toY < 0 || toY >= field(0).length then
    false
  else
    field(fromX)(fromY) match
      case '|' =>
        val xDiff = toX - fromX
        toY == fromY && xDiff >= -1 && xDiff <= 1

      case '-' =>
        val yDiff = toY - fromY
        toX == fromX && yDiff >= -1 && yDiff <= 1

      case 'L' =>
        (toY == fromY && toX == fromX - 1) || (toX == fromX && toY == fromY + 1)

      case 'J' =>
        (toY == fromY && toX == fromX - 1) || (toX == fromX && toY == fromY - 1)

      case '7' =>
        (toY == fromY && toX == fromX + 1) || (toX == fromX && toY == fromY - 1)

      case 'F' =>
        (toY == fromY && toX == fromX + 1) || (toX == fromX && toY == fromY + 1)

      case '.' =>
        false

      case 'S' =>
        connectedTo(field)(to, from)
  end if
end connectedTo

def getNeighbours(field: Field)(point: Point): List[Point] =
  val (x, y) = point

  val candidates = List(
                (x - 1, y),
    (x, y - 1), /* point */ (x, y + 1),
                (x + 1, y)
  )

  candidates.filter(c => connectedTo(field)(point, c))
end getNeighbours

def buildPath(field: Field): List[Point] =
  @tailrec
  def go(point: Point, acc: List[Point]): List[Point] =
    val (x, y) = point

    if field(x)(y) == 'S' && acc.nonEmpty then
      acc
    else
      val nextPoint = getNeighbours(field)(point)
        .filterNot(acc.headOption contains _)
        .head
      go(nextPoint, point :: acc)
  end go

  val startX = field.indexWhere(_.contains('S'))
  val startY = field(startX).indexOf('S')

  go((startX, startY), Nil)
end buildPath

private def parseInput(input: Seq[String]): Field =
  input.map(_.toVector).toVector
