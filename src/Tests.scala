// little test suite

object Tests extends App {

  val patterns = List("abc", "(abc)", "a?b", "ab?", "(a|b)c?", "I (like|love|hate)( (cat|dog))? people", "((h|j)ell. worl?d)|(42)")
  val strings  = List(
    List("abc", "ab", "ac", "bc", "a", "b", "c"),
    List("abc", "b", "c", "ab", "bc", "ac", "a"),
    List("ab", "b", "a", "aa", "bb", "abb"),
    List("a", "ab", "b", "aa", "bb", "abb"),
    List("a", "b", "ac", "bc", "abc", "ab", "c"),
    List("I like cat people", "I love dog people", "I hate people", "I likel people", "I people"),
    List("hello world", "jello word", "jelly word", "42", "24", "hello world42")
  )
  val expected = List(
    List(true, false, false, false, false, false, false),
    List(true, false, false, false, false, false, false),
    List(true, true, false, false, false, false),
    List(true, true, false, false, false, false),
    List(true, true, true, true, false, false, false),
    List(true, true, true, false, false),
    List(true, true, true, true, false, false)
  )

  // recursive tests
  @scala.annotation.tailrec
  def tests(patterns: List[String], strings: List[List[String]], expected: List[List[Boolean]]): Unit = {
    val pat  = patterns.headOption
    val strs = strings.headOption
    if (pat.isDefined && strs.isDefined) {
      val pattern = new RecursiveDescentParser(pat.get).parseS()
      val teststr = strs.get.map(new RecursiveDescentParser(_).parseS())
      val result  = teststr.map(pattern.matches)
      val asExpected = result == expected.head
      println()
      println(if (asExpected) "Yay!!!" else "***Something's fishy***")
      println(s"Pattern: ${pat.get}")
      println(s"Strings: ${strs.get}")
      println("Result: " + result)
      println("Expect: " + expected.head)
      tests(patterns.tail, strings.tail, expected.tail)
    }
  }
  tests(patterns, strings, expected)
}
