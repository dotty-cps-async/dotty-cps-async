//> using scala 3.9.0

case class Person(name: String, age: Int)

@main def test(): Unit =
  // Named pattern is typed as Unapply(fun, Nil, List(Wildcard(), NamedArg("age", Bind("a", Wildcard())))).
  // TreeMap.transformTree handles NamedArg as a term, and TreeMap.transformTerm for NamedArg calls
  // transformTerm on the pattern, which fails:
  //   scala.MatchError: Bind("a", Wildcard()) (of class java.lang.String)
  //     at scala.quoted.Quotes$reflectModule$TreeMap.transformTerm
  // Reproduced with 3.7.4, 3.8.4, 3.9.0.
  val result = TestMacro.identityTreeMap {
    Person("b", 5) match
      case Person(age = a) => a + 1
  }
  assert(result == 6)
  println("Test passed!")
