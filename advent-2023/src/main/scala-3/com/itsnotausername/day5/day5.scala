package com.itsnotausername.day5

import com.itsnotausername.files.loadFile

@main def part1(): Unit =
  val input  = loadFile("day5/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")
end part1

@main def part2(): Unit =
  val input  = loadFile("day5/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")
end part2

private def makeAnswerPart1(input: Seq[String]): Long =
  val (seeds, mappings) = parseInput(input)
  val ranges = seeds.map(seed => Range(seed, seed))

  makeAnswer(ranges, mappings)
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Long =
  val (seeds, mappings) = parseInput(input)
  val ranges = seeds
    .grouped(2)
    .map { case List(start, length) => Range(start, start + length - 1) }
    .toList

  makeAnswer(ranges, mappings)
end makeAnswerPart2

private def makeAnswer(ranges: List[Range], mappings: List[Mappings]): Long =
  @annotation.tailrec
  def process(
    ranges: List[Range],
    mappings: List[RangeMapping],
    acc: List[Range] = Nil
  ): List[Range] =
    if mappings.isEmpty then
      (acc ::: ranges).sorted
    else if ranges.isEmpty then
      acc.sorted
    else
      val mapping = mappings.head
      val range = ranges.head

      // mapping: |-----|
      //
      // range:           |-----|
      if mapping.range.end < range.start then
        process(ranges, mappings.tail, acc)

      // mapping:         |-----|
      //
      // range:   |-----|
      else if mapping.range.start > range.end then
        process(ranges.tail, mappings, range :: acc)

      // mapping:    |-----|
      //
      // range:   |-----|
      else if mapping.range.start > range.start then
        val rangeToAdd = Range(range.start, mapping.range.start - 1)
        val rangeToProcess = Range(mapping.range.start, range.end)

        process(rangeToProcess :: ranges.tail, mappings, rangeToAdd :: acc)

      else
        val rangeToAdd = Range(range.start, mapping.range.end min range.end).shift(mapping.delta)
        val updatedRanges =
          // mapping: |---------|
          //
          // range:     |-----|
          if range.end <= mapping.range.end then
            ranges.tail
          // mapping: |-----|
          //
          // range:     |---------|
          else
            Range(mapping.range.end + 1, range.end) :: ranges.tail

        process(updatedRanges, mappings, rangeToAdd :: acc)
  end process

  val sortedRanges = ranges.sorted

  mappings
    .foldLeft(sortedRanges) {
      case (prevRanges, mapping) =>
        val proc = process(prevRanges, mapping.ranges)
        println(proc)
        proc
    }
    .minBy(_.start)
    .start
end makeAnswer

final case class Range(start: Long, end: Long):
  def shift(delta: Long): Range = Range(start + delta, end + delta)
end Range

object Range:
  given Ordering[Range] = Ordering
    .fromLessThan { (r1, r2) =>
      if r1.start == r2.start then
        r1.end < r2.end
      else
        r1.start < r2.start
    }
end Range

final case class RangeMapping(range: Range, delta: Long)

object RangeMapping:
  def from(srcStart: Long, dstStart: Long, length: Long): RangeMapping =
    val range = Range(srcStart, srcStart + length - 1)
    val delta = dstStart - srcStart
    apply(range, delta)
  end from
end RangeMapping

final case class Mappings(ranges: List[RangeMapping])

object Mappings:
  def from(ranges: List[RangeMapping]): Mappings =
    val sortedRanges = ranges.sortBy(_.range)
    apply(sortedRanges)
  end from
end Mappings

private def parseInput(input: Seq[String]): (List[Long], List[Mappings]) =
  val seeds = input.head match
    case s"seeds: $seedStr" =>
      seedStr.split("\\s+").map(_.toLong).toList

  val (_, mappings) = (input.tail :+ "")
    .foldLeft((List.empty[RangeMapping], List.empty[Mappings])) {
      case ((ranges, mappings), row) =>
        val updatedMappings =
          if row.isBlank && ranges.nonEmpty then
            Mappings.from(ranges) :: mappings
          else
            mappings

        val updatedRanges =
          if row.isBlank || row.contains("map") then
            Nil
          else
            val Seq(dstStart, srcStart, length) = row.split("\\s+").map(_.toLong).toList
            RangeMapping.from(srcStart, dstStart, length) :: ranges

        (updatedRanges, updatedMappings)
    }

  (seeds, mappings.reverse)
end parseInput
