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
        .getOrElse(Stop())
    }
  }

  case class Stop(value: String = "NONE", nextState: Option[VowelState] = None) extends VowelState

  case class UState(value: String = "u", nextState: Option[VowelState] = Some(Stop())) extends VowelState

  case class OState(value: String = "o", nextState: Option[VowelState] = Some(UState())) extends VowelState

  case class IState(value: String = "i", nextState: Option[VowelState] = Some(OState())) extends VowelState

  case class EState(value: String = "e", nextState: Option[VowelState] = Some(IState())) extends VowelState

  case class AState(value: String = "a", nextState: Option[VowelState] = Some(EState())) extends VowelState


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
        .flatMap(tuple => tuple match {
          case _ if stringStartsWithAnA(tuple._1, s) => List(StartIndex)
          case _ if newStartOfA(tuple, s) => List(tuple._2)
          case _ => Nil
        })
        .map(indices => vowelOf(s.substring(indices)))
        .filter(_.endsWith(UState().value))
        .map(_.size)
        .maxOption
        .getOrElse(ZeroLength)
    }
  }

  private def stringStartsWithAnA(index: Int, s: String): Boolean = {
    index == StartIndex && s.charAt(index).toString == AState().value
  }

  private def newStartOfA(indices: (Int, Int), s: String): Boolean = {
    s.charAt(indices._1).toString != AState().value && s.charAt(indices._2).toString == AState().value
  }

  private def vowelOf(part: String): String = {
    def traverseString(part: String, currentState: VowelState): String = {
      part.headOption match {
        case Some(letter) if currentState.shouldAddLetter(letter) && !part.tail.isEmpty => letter.toString ++ traverseString(part.tail, currentState.nextState(letter))
        case Some(letter) if currentState.shouldAddLetter(letter) => letter.toString
        case Some(letter) => traverseString(part.tail, currentState.nextState(letter))
        case None => EmptyString
      }
    }

    traverseString(part, AState())
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
