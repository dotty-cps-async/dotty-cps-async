//> using scala 3.3.7
//> using scalacOption -Xcheck-macros

case class P(x: Int, y: Int)

@main def test(): Unit =
  // Identity TreeMap over a tree with case class copy + default getter
  // triggers "-Xcheck-macros" assertion failure:
  //   "symbols differ for p.copy$default$1"
  // TreeChecker fails to resolve copy$default$N as a member when
  // the qualifier is a TermRef to a local val.
  val result = TestMacro.identityTreeMap {
    val p = P(3, 4)
    p.copy(y = 7)
  }
  assert(result == P(3, 7))
  println("Test passed!")
