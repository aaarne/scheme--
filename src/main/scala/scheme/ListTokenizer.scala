package scheme

class ListTokenizer(s: String) extends Iterator[String] {
  private var i = 0
  
  private def isDelimiter(ch: Char) = 
    ch <= ' ' || ch == '(' || ch == ')'

  def hasNext: Boolean = {
    while (i < s.length && s.charAt(i) <= ' ') {
      i = i + 1
    }
    i < s.length
  }

  def next: String = 
  if (hasNext) {
    val start = i
    var ch = s.charAt(i)
    i = i + 1
    if (ch == '(') "("
    else if (ch == ')') ")"
    else {
      while (i < s.length && !isDelimiter(s.charAt(i))) {i = i+1}
      s.substring(start, i)
    }
  } else throw new RuntimeException("more input expected")
}