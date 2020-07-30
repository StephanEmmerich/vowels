import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Result {

  sealed trait State {
    val value: String
    val nextState: State

    def shouldAddLetter(letter: Char): Boolean = {
      letter.toString == value || nextState.value == letter.toString
    }

    def nextState(letter: Char): State = {
      letter match {
        case nothing if nextState == null => Stop()
        case next if nextState.value == letter.toString => nextState
        case _ => this
      }
    }
  }

  case class Stop(value: String = "NONE", nextState: State = null) extends State

  case class UState(value: String = "u", nextState: State = Stop()) extends State

  case class OState(value: String = "o", nextState: State = UState()) extends State

  case class IState(value: String = "i", nextState: State = OState()) extends State

  case class EState(value: String = "e", nextState: State = IState()) extends State

  case class AState(value: String = "a", nextState: State = EState()) extends State


  /*
   * Complete the 'longestVowelSubsequence' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts STRING s as parameter.
   */

  def longestVowelSubsequence(s: String): Int = {
    val results: ArrayBuffer[List[String]] = new ArrayBuffer[List[String]]()
    for (i <- 0 to (s.length - 1)) {
      if (s(i).toString == AState().value) {
        results.append(vowelOf(s.substring(i)))
      }
    }
    results
      .flatten
      .map(_.size)
      .maxOption
      .getOrElse(0)
  }

  private def vowelOf(part: String): List[String] = {
    def traverseString(part: String, currentState: State): String = {
      part.headOption match {
        case Some(letter) if currentState.shouldAddLetter(letter) && !part.tail.isEmpty => letter.toString ++ traverseString(part.tail, currentState.nextState(letter))
        case Some(letter) if currentState.shouldAddLetter(letter) => letter.toString
        case Some(letter) => traverseString(part.tail, currentState.nextState(letter))
        case None => ""
      }
    }

    val parsedPart = traverseString(part, AState())
    if (parsedPart.endsWith(UState().value)) List(parsedPart)
    else Nil
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
