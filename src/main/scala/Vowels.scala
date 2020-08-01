import java.io.PrintWriter

import scala.io.StdIn

object Result {

  val NumberOfVowels = 5
  val AsPair = 2
  val ZeroLength = 0
  val StartIndex = 0
  val EmptyString = ""

  sealed trait VowelState {
    val value: String
    val nextState: Option[VowelState]

    def shouldAddLetter(letter: Char): Boolean = {
      letter.toString == value || nextState.map(_.value == letter.toString).getOrElse(false)
    }

    def nextState(letter: Char): VowelState = {
      nextState
        .map(ns =>
          if (ns.value == letter.toString) ns
          else this)
        .getOrElse(Stop)
    }
  }

  case object Stop extends VowelState {
    override val value: String = "NONE"
    override val nextState: Option[VowelState] = None
  }

  case object UState extends VowelState {
    override val value: String = "u"
    override val nextState: Option[VowelState] = Some(Stop)
  }

  case object OState extends VowelState {
    override val value: String = "o"
    override val nextState: Option[VowelState] = Some(UState)
  }

  case object IState extends VowelState {
    override val value: String = "i"
    override val nextState: Option[VowelState] = Some(OState)
  }

  case object EState extends VowelState {
    override val value: String = "e"
    override val nextState: Option[VowelState] = Some(IState)
  }

  case object AState extends VowelState {
    override val value: String = "a"
    override val nextState: Option[VowelState] = Some(EState)
  }

  case class OrderedVowels(vowels: String = "", currentState: VowelState = AState) {

    lazy val endsWithUState = currentState == UState

    def mapCharacter(vowel: Char): OrderedVowels = {
      vowel match {
        case _ if currentState.shouldAddLetter(vowel) => OrderedVowels(vowels + vowel, currentState.nextState(vowel))
        case _ => OrderedVowels(vowels, currentState.nextState(vowel))
      }
    }

  }

  /*
   * Complete the 'longestVowelSubsequence' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts STRING s as parameter.
   */

  def longestVowelSubsequence(s: String): Int = {
    if (s.length < NumberOfVowels) ZeroLength
    else {
      (StartIndex until s.length)
        .sliding(AsPair)
        .map(toTuples => (toTuples.head, toTuples.tail.head))
        .flatMap(tuple => findAllStartIndicesForVowelCount(tuple, s))
        .map(startIndex => vowelOf(s.substring(startIndex)))
        .filter(_.endsWithUState)
        .map(_.vowels.size)
        .maxOption
        .getOrElse(ZeroLength)
    }
  }

  private def findAllStartIndicesForVowelCount(tuple: (Int, Int), s: String): List[Int] = {
    tuple match {
      case _ if stringStartsWithAnA(tuple._1, s) => List(StartIndex)
      case _ if newStartOfA(tuple, s) => List(tuple._2)
      case _ => Nil
    }
  }

  private def stringStartsWithAnA(index: Int, s: String): Boolean = {
    index == StartIndex && s.charAt(index).toString == AState.value
  }

  private def newStartOfA(indices: (Int, Int), s: String): Boolean = {
    s.charAt(indices._1).toString != AState.value && s.charAt(indices._2).toString == AState.value
  }

  private def vowelOf(part: String): OrderedVowels = {
    part.foldLeft(OrderedVowels())((orderedVowels, letter) => orderedVowels.mapCharacter(letter))
  }
}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val s = StdIn.readLine

    val result = Result.longestVowelSubsequence(s)

    printWriter.println(result)

    printWriter.close()
  }
}
