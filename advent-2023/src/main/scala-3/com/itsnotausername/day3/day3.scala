package com.itsnotausername.day3

import com.itsnotausername.files.loadFile

import scala.collection.immutable.{SortedMap, SortedSet}

@main def part1(): Unit =
  val input  = loadFile("day3/input.txt")
  val answer = makeAnswerPart1(input)

  println(s"The answer is $answer")

@main def part2(): Unit =
  val input  = loadFile("day3/input.txt")
  val answer = makeAnswerPart2(input)

  println(s"The answer is $answer")

private def makeAnswerPart1(input: Seq[String]): Int =
  def checkNumber(number: Number, symbolMap: Map[Int, SortedSet[Int]]): Int =
    val overlap = (number.startX - 1 to number.startX + 1)
      .exists { x =>
        val xSymbols = symbolMap(x)

        xSymbols
          .minAfter(number.startY - 1)
          .exists(_ <= number.startY + number.length)
      }

    if overlap then number.value else 0
  end checkNumber

  val (numbers, symbols) = parseSchematic(input)

  val symbolMap = symbols
    .groupMap(_.posX)(_.posY)
    .map { case (key, value) => key -> SortedSet.from(value) }
    .withDefaultValue(SortedSet.empty[Int])

  numbers.map(checkNumber(_, symbolMap)).sum
end makeAnswerPart1

private def makeAnswerPart2(input: Seq[String]): Int =
  def checkSymbol(symbol: Symbol, numberMap: Map[Int, SortedMap[Int, Number]]): Int =
    if symbol.value != '*' then
      0
    else
      val adjacentNumbers = (symbol.posX - 1 to symbol.posX + 1)
        .iterator
        .flatMap { x =>
          val xNumbers = numberMap(x)

          xNumbers
            .valuesIteratorFrom(symbol.posY)
            .takeWhile { num =>
              val leftBound  = num.startY <= symbol.posY + 1
              val rightBound = num.startY + num.length >= symbol.posY

              leftBound && rightBound
            }
            .map(_.value)
        }

      val maybeProduct = for
        fstNumber <- adjacentNumbers.nextOption()
        sndNumber <- adjacentNumbers.nextOption()
        if !adjacentNumbers.hasNext
      yield fstNumber * sndNumber

      maybeProduct.getOrElse(0)
    end if
  end checkSymbol

  val (numbers, symbols) = parseSchematic(input)

  val numberMap = numbers
    .groupBy(_.startX)
    .map { case (key, value) => key -> SortedMap.from(value.map(n => (n.startY + n.length) -> n)) }
    .withDefaultValue(SortedMap.empty[Int, Number])

  symbols.map(checkSymbol(_, numberMap)).sum
end makeAnswerPart2

final case class Number(value: Int, startX: Int, startY: Int, length: Int)
final case class Symbol(value: Char, posX: Int, posY: Int)

private def parseSchematic(input: Seq[String]): (Seq[Number], Seq[Symbol]) =
  def parseRow(row: String, rowNumber: Int): (List[Number], List[Symbol]) =
    row
      .foldLeft(ParsedState(rowNumber)) { case (state, char) => state.processChar(char) }
      .result

  val (_, numbers, symbols) = input.foldLeft((0, List.empty[Number], List.empty[Symbol])) {
    case ((rowNumber, numbers, symbols), row) =>
      val (rowNumbers, rowSymbols) = parseRow(row, rowNumber)

      (rowNumber + 1, rowNumbers ::: numbers, rowSymbols ::: symbols)
  }

  (numbers, symbols)
end parseSchematic

final case class ParsedState(
  rowNumber: Int,
  currentPos: Int       = 0,
  parsedNum: String     = "",
  parsedNumStart: Int   = -1,
  numbers: List[Number] = Nil,
  symbols: List[Symbol] = Nil
):
  self =>

    def result: (List[Number], List[Symbol]) = (updatedNumbers, symbols)

    def processChar(char: Char): ParsedState =
      if char.isDigit then
        processDigit(char)
      else if char != '.' then
        processSymbol(char)
      else
        processNoSymbol

    private def processDigit(digit: Char): ParsedState =
      val correctStart = if parsedNum.isEmpty then currentPos else parsedNumStart
      copy(
        currentPos     = updatedPos,
        parsedNum      = self.parsedNum + digit,
        parsedNumStart = correctStart
      )

    private def processSymbol(symbol: Char): ParsedState =
      val smb = Symbol(symbol, rowNumber, currentPos)
      copy(
        currentPos     = updatedPos,
        parsedNum      = "",
        parsedNumStart = -1,
        numbers        = updatedNumbers,
        symbols        = smb :: self.symbols
      )

    private def processNoSymbol: ParsedState =
      copy(
        currentPos     = updatedPos,
        parsedNum      = "",
        parsedNumStart = -1,
        numbers        = updatedNumbers
      )

    private def updatedPos: Int = currentPos + 1

    private def updatedNumbers: List[Number] =
      if parsedNum.isEmpty then
        numbers
      else
        val num = Number(parsedNum.toInt, rowNumber, parsedNumStart, parsedNum.length)
        num :: numbers

end ParsedState
