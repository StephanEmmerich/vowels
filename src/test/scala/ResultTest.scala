import org.scalatest.flatspec.AnyFlatSpec

class VowelsTest extends AnyFlatSpec {

  "Empty string" should "be 0" in {
    assertResult(0)(Result.longestVowelSubsequence(""))
  }

  "aeiou" should "be 6" in {
    assertResult(5)(Result.longestVowelSubsequence("aeiou"))
  }

  "aeiiiaaiiiiaaaaaiiiiiiooooooaaaoooouuuuueeeuuuuuuuuuu" should "be 40" in {
    assertResult(40)(Result.longestVowelSubsequence("aeiiiaaiiiiaaaaaiiiiiiooooooaaaoooouuuuueeeuuuuuuuuuu"))
  }

  "eouiuaaeiaoauaoaeauieeoiuiuiu" should "be 10" in {
    assertResult(10)(Result.longestVowelSubsequence("eouiuaaeiaoauaoaeauieeoiuiuiu"))
  }

}
