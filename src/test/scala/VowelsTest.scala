import org.scalatest.flatspec.AnyFlatSpec

class VowelsTest extends AnyFlatSpec {

  "Empty string" should "be 0" in {
    assertResult(0)(Result.longestVowelSubsequence(""))
  }

  "aeiou" should "be 5" in {
    assertResult(5)(Result.longestVowelSubsequence("aeiou"))
  }

  "aeiouaeiou" should "be 6 (aeiouu)" in {
    assertResult(6)(Result.longestVowelSubsequence("aeiouaeiou"))
  }

  "aeiiiaaiiiiaaaaaiiiiiiooooooaaaoooouuuuueeeuuuuuuuuuu" should "be 40" in {
    assertResult(40)(Result.longestVowelSubsequence("aeiiiaaiiiiaaaaaiiiiiiooooooaaaoooouuuuueeeuuuuuuuuuu"))
  }

  "eouiuaaeiaoauaoaeauieeoiuiuiu" should "be 10" in {
    assertResult(10)(Result.longestVowelSubsequence("eouiuaaeiaoauaoaeauieeoiuiuiu"))
  }

}
