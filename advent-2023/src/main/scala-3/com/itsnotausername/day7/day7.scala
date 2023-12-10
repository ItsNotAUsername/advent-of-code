package com.itsnotausername.day7

import com.itsnotausername.day7.Hand.defaultCardPriority
import com.itsnotausername.files.loadFile

import scala.math.Ordered.orderingToOrdered

@main def part1(): Unit =
  val input  = loadFile("day7/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day7/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswerPart1(input: Seq[String]): Long =
  def rateHand(hand: Hand): Int =
    val maxOfTheSame = hand.cards.values.max
    val numOfGroups  = hand.cards.size

    handType(maxOfTheSame, numOfGroups)
  end rateHand

  val hands = parseInput(input)
  makeAnswer(hands, rateHand, defaultCardPriority)
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Long =
  val jokerCard = 'J'

  def rateHand(hand: Hand): Int =
    val cards = hand.cards

    val jokerCount = cards.getOrElse(jokerCard, 0)

    val (_, maxOfTheSame) = cards.maxBy { case (card, cnt) => if card == jokerCard then -1 else cnt }
    val numOfGroups       = if jokerCount == 0 then cards.size else cards.size - 1

    handType(maxOfTheSame + jokerCount, numOfGroups)
  end rateHand

  val cardPriority = defaultCardPriority + (jokerCard -> 1)

  val hands = parseInput(input)
  makeAnswer(hands, rateHand, cardPriority)
end makeAnswerPart2

private def makeAnswer(hands: List[Hand], handType: Hand => Int, cardPriority: Char => Int): Long =
  given Ordering[Hand] = Ordering
    .fromLessThan { case (h1, h2) =>
      val tpe1 = handType(h1)
      val tpe2 = handType(h2)

      if tpe1 == tpe2 then
        h1.value.map(cardPriority) < h2.value.map(cardPriority)
      else
        tpe1 < tpe2
    }

  hands
    .sorted
    .zipWithIndex
    .foldLeft(0L) {
      case (sum, (hand, rank)) => sum + hand.bid * (rank + 1)
    }
end makeAnswer

private def handType(maxOfTheSame: Int, numOfGroups: Int): Int =
  // High card
  if numOfGroups == 5 then
    0
  // One pair
  else if numOfGroups == 4 then
    1
  // Two pair
  else if numOfGroups == 3 && maxOfTheSame == 2 then
    2
  // Three of a kind
  else if numOfGroups == 3 && maxOfTheSame == 3 then
    3
  // Full house
  else if numOfGroups == 2 && maxOfTheSame == 3 then
    4
  // Four of a kind
  else if maxOfTheSame == 4 then
    5
  // Five of a kind
  else
    6
end handType

final case class Hand(value: String, bid: Long):
  lazy val cards: Map[Char, Int] =
    value.groupMapReduce(identity)(_ => 1)(_ + _)
end Hand

object Hand:
  val defaultCardPriority: Map[Char, Int] =
    Map(
      'A' -> 14,
      'K' -> 13,
      'Q' -> 12,
      'J' -> 11,
      'T' -> 10,
      '9' -> 9,
      '8' -> 8,
      '7' -> 7,
      '6' -> 6,
      '5' -> 5,
      '4' -> 4,
      '3' -> 3,
      '2' -> 2,
    )
end Hand

private def parseInput(input: Seq[String]): List[Hand] =
  input
    .map {
      case s"$hand $bid" => Hand(hand, bid.trim.toLong)
    }
    .toList
end parseInput
