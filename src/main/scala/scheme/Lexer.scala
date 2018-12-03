package scheme

trait Lexer {

  type Data = Any

  class LispTokenizer(s: String) extends Iterator[String] {

    private var i = 0

    private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'

    def hasNext: Boolean = {
      while (i < s.length && s.charAt(i) <= ' ') {
        i += 1
      }
      i < s.length
    }

    def next: String =
      if (hasNext) {
        val start = i
        if (isDelimiter(s.charAt(i))) {
          i += 1
        }
        else {
          do {
            i += 1
          } while (i < s.length && !isDelimiter(s.charAt(i)))
        } 
        s.substring(start, i)
      } else sys.error("premature end of input")
  }

  def string2lisp(s: String): Data = {

    val it = new LispTokenizer(s)

    def parseExpr(token: String): Data = {
      if (token == "(")
        parseList
      else if (token == ")")
        sys.error("unbalanced parentheses")
      else if (token.matches("^-?\\d+$"))
        Integer.parseInt(token)
      else
        Symbol(token)
    }

    def parseList: List[Data] = {
      val token = it.next
      if (token == ")")
        List()
      else
        parseExpr(token) :: parseList
    }
    parseExpr(it.next)
  }

  def lisp2string(x: Data): String = x match {
    case Symbol(name) =>
      name
    case xs: List[_] =>
      xs.map(lisp2string).mkString("(", " ", ")")
    case _ =>
      x.toString
  }
}