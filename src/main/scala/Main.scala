import scheme._

object Main extends App {
  
  val program = """
  (def factorial 
    (lambda (n)
      (if (= n 0)
        1
        (* n (factorial (- n 1)))))
    (factorial 5))"""


  println(s"Program: $program\n")

  val tokens = (for (s <- new ListTokenizer(program)) yield s).toList

  println(s"Tokens: $tokens\n")

  val parsed = Scheme.parseString(program)

  println(s"Parsed: $parsed\n")

  println(s"Decompiled: ${Scheme.decompile(parsed)}")

  val result = Scheme.evaluate(parsed)

  println(s"Evaluated result: $result")
}
