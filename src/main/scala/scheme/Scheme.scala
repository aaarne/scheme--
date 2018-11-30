package scheme 

class Scheme {

  type Data = Any
  case class Lambda(f: PartialFunction[List[Data], Data])

  def parseString(s: String): Data = {
    val it = new ListTokenizer(s)
    
    def parseExpr(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") throw new RuntimeException("unmatched paranthesis")
      else if (token.matches("^-?\\d+$")) token.toInt
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

  var curexp: Data = null

  def evaluate(x: Data): Data = eval(x, globalEnv)

  def evaluate(s: String): Data = evaluate(decompile(s))

  def eval(x: Data, env: Environment): Data = {
    val prevexp = curexp
    curexp = x
    
    val result = eval1(x, env)
    curexp = prevexp
    result
  }

  def asList(x: Data): List[Data] = x match {
    case xs: List[_] => xs
  }

  def paramName(x: Data): String = x match {
    case Symbol(name) => name
  }


abstract class Environment {
  def lookup(n: String): Data
  def extend(name: String, v: Data): Environment = {
    val encl = this
    new Environment {
      def lookup(n: String): Data = 
        if (n == name) v else encl.lookup(n)
    }
  }
  def extendMulti(ps: List[String], vs: List[Data]): Environment = (ps, vs) match {
    case (Nil, Nil) => this
    case (p :: ps1, arg :: args1) => extend(p, arg).extendMulti(ps1, args1)
    case _ => throw new IllegalArgumentException("wrong number of arguments")
  }

  def extendRec(name: String, expr: Environment => Data): Environment = {
    val encl = this
    new Environment {
      def lookup(n: String): Data =
        if (n == name) expr(this) else encl.lookup(n)
    }
  }
}

object EmptyEnvironment extends Environment {
  def lookup(n: String): Data = throw new RuntimeException(s"$n does not exist.")
}

  var globalEnv = EmptyEnvironment
    .extend("=", Lambda {
      case List(arg1, arg2) => if (arg1 == arg2) 1 else 0
    })
    .extend("+", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 + arg2
    })
    .extend("-", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 - arg2
    })
    .extend("*", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 * arg2
    })
    .extend("/", Lambda {
      case List(arg1: Int, arg2: Int) => arg1 / arg2
    })
    .extend("nil", Nil)
    .extend("cons", Lambda {
      case List(arg1, arg2) => arg1 :: asList(arg2)
    })
    .extend("car", Lambda {
      case List(x :: xs) => x
    })
    .extend("cdr", Lambda {
      case List(x :: xs) => xs
    })
    .extend("null?", Lambda {
      case List(Nil) => 1
      case _ => 0
    })

  def eval1(x: Data, env: Environment): Data = x match {
    case _: Int => x
    case Symbol(name) => env.lookup(name)
    case 'val :: Symbol(name) :: expr :: rest :: Nil =>
      eval(rest, env.extend(name, eval(expr, env)))
    case 'if :: cond :: thenpart :: elsepart :: Nil =>
      if (eval(cond, env) != 0) eval(thenpart, env)
      else (elsepart, env)
    case 'and :: x :: y :: Nil => eval('if :: x :: y :: 0 :: Nil, env)
    case 'or :: x :: y :: Nil => eval('if :: x :: 1 :: y :: Nil, env)
    case 'def :: Symbol(name) :: body :: Nil if env == globalEnv => 
      globalEnv = env.extendRec(name, env1 => eval(body, env1))
    case 'def :: Symbol(name) :: body :: rest :: Nil => 
      if (env == globalEnv) globalEnv = env.extendRec(name, env1 => eval(body, env1))
      eval(rest, env.extendRec(name, env1 => eval(body, env1)))
    case 'quote :: y :: Nil => y
    case 'lambda :: params :: body :: Nil => 
      mkLambda(asList(params) map paramName, body, env)
    case operator :: operands =>
      apply(eval(operator, env), operands map (x => eval(x, env)))
  }

  def mkLambda(ps: List[String], body: Data, env: Environment): Lambda = 
    Lambda {
      case args => eval(body, env.extendMulti(ps, args))
    }

  def apply(f: Data, args: List[Data]): Data = f match {
    case Lambda(f) => f(args)
  }
}

object Scheme extends Scheme