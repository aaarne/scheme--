package scheme 

class Scheme {

  type Data = Any

  def parseString(s: String): Data = {
    val it = new ListTokenizer(s)
    
    def parseExpr(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") throw new RuntimeException("unmatched paranthesis")
      else if (token.charAt(0).isDigit || token.charAt(0) == '-') token.toInt
      else if (token.charAt(0) == '\"' && token.charAt(token.length-1) == '\"') token.substring(1, token.length-1)
      else Symbol(token)
    }

    def parseList: List[Data] = {
      val token = it.next
      if (token == ")") Nil else parseExpr(token) :: parseList
    }

    parseExpr(it.next)
  }

  def decompile(lisp: Data): String = lisp match {
    case Symbol(x) => x
    case xs: List[_] => (xs map decompile).mkString("(", " ", ")")
    case _ => lisp.toString
  }

}

object Scheme extends Scheme