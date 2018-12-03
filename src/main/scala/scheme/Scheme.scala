package scheme

trait Scheme {

  type Data = Any
  case class Lambda(f: PartialFunction[List[Data], Data])

  def eval(x: Data, env: Environment): Data

  def asList(x: Data): List[Data] = x match {
    case xs: List[_] => xs
    case _ => sys.error(s"expected a list but input was $x")
  }

  def paramName(x: Data): String = x match {
    case Symbol(name) => name
    case _ => sys.error(s"malformed parameter: $x")
  }

  def mkLambda(ps: List[String], body: Data, env: Environment): Lambda =
    Lambda { case args =>
      eval(body, env.extendMulti(ps, args))
    }

  def apply(f: Data, args: List[Data]): Data = f match {
    case Lambda(f) => f(args)
    case _ => sys.error(s"application of non-function $f to arguments $args")
  }

  abstract class Environment {
    def lookup(n: String): Data
    def toString: String
    def extend(name: String, v: Data): Environment = {
      val enclosingEnvironment = this
      new Environment {
        def lookup(n: String): Data =
          if (n == name) v else enclosingEnvironment.lookup(n)

        override def toString: String = s"$name: $v\n" + enclosingEnvironment.toString
      }
    }
    def extendMulti(ps: List[String], vs: List[Data]): Environment = (ps, vs) match {
      case (List(), List()) => this
      case (p :: ps1, arg :: args1) => extend(p, arg).extendMulti(ps1, args1)
      case _ => sys.error("wrong number of arguments")
    }
    def extendRec(name: String, expr: Environment => Data): Environment = {
      val enclosingEnvironment = this
      new Environment {
        def lookup(n: String): Data =
          if (n == name) expr(this)
          else enclosingEnvironment.lookup(n)

        override def toString: String = s"$name: ${expr(this)}\n" + enclosingEnvironment.toString
      }
    }
  }

  object EmptyEnvironment extends Environment {
    def lookup(n: String): Data = sys.error("undefined: " + n + ", this can happen when the wrong number of arguments is passed")
    override def toString(): String = "" 
  }

  var globalEnv = EmptyEnvironment
    .extend("=", Lambda {case List(arg1, arg2) => if (arg1 == arg2) 1 else 0})
    .extend("+", Lambda {case List(arg1: Int, arg2: Int) => arg1 + arg2})
    .extend("-", Lambda {case List(arg1: Int, arg2: Int) => arg1 - arg2})
    .extend("*", Lambda {case List(arg1: Int, arg2: Int) => arg1 * arg2})
    .extend("/", Lambda {case List(arg1: Int, arg2: Int) => arg1 / arg2})
    .extend("nil", Nil)
    .extend("cons", Lambda {case List(arg1, arg2) => arg1 :: asList(arg2)})
    .extend("car", Lambda {case List(x :: xs) => x})
    .extend("cdr", Lambda {case List(x :: xs) => xs})
    .extend("map", Lambda {case List(l: List[Data], f) => l map (e => apply(f, e :: Nil))})
    .extend("null?", Lambda {
      case List(Nil) => 1
      case _ => 0
    })
}