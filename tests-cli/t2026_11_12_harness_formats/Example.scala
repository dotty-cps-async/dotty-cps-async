//> using scala 3.9.0
//> using options "-Wunused:imports,privates" -Werror
// Not a reproduction: checks the formats supported by cc.TestsCli (a quoted option with a comma,
// the package of the @main method, @main on its own line).
package example

@main
def probe(): Unit =
  println("ok")
