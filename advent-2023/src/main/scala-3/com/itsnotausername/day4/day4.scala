package com.itsnotausername.day4

import com.itsnotausername.files.loadFile

@main def part1(): Unit =
  val input  = loadFile("day4/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")

@main def part2(): Unit =
  val input = loadFile("day4/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")

private def makeAnswerPart1(input: Seq[String]): Int =
  def countPoints(card: Card): Int =
    card.available.foldLeft(0) { case (points, number) =>
      if card.winning.contains(number) then
        if points > 0 then points * 2 else 1
      else
        points
    }

  val cards = parseCards(input)
  cards.foldLeft(0)(_ + countPoints(_))
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Int =
  def countMatches(card: Card): Int =
    card.available.count(card.winning.contains)

  def countScratchcards(cards: Seq[Card]): Int =
    val map = Map.empty[Int, Int].withDefaultValue(1)

    val (cnt, _) = cards.foldLeft((0, map)) {
      case ((currCnt, currMap), card) =>
        val cardCnt = currMap(card.id)
        val matches = countMatches(card)

        val updatedMap = (1 to matches)
          .foldLeft(currMap) { case (map, diff) =>
            map.updatedWith(card.id + diff) {
              case Some(value) => Some(value + cardCnt)
              case None        => Some(1 + cardCnt)
            }
          }

        (currCnt + cardCnt, updatedMap)
    }

    cnt
  end countScratchcards

  val cards = parseCards(input)
  countScratchcards(cards)
end makeAnswerPart2

final case class Card(id: Int, winning: List[Int], available: List[Int])

private def parseCards(input: Seq[String]): Seq[Card] =
  def parseRow(row: String): Card =
    val Array(idStr, numberStr) = row.split(":")

    val id = idStr
      .drop(4)
      .trim
      .toInt

    val Seq(winning, available) = numberStr
      .split("\\|")
      .map(_.trim.split("\\s+").map(_.toInt).toList)
      .toSeq

    Card(id, winning, available)
  end parseRow

  input.map(parseRow)
end parseCards
