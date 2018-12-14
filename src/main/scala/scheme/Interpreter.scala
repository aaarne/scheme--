package scheme

// scalafix:off

object Interpreter extends Scheme with Lexer {

  type Data = Any

  def evaluate(x: Data): Data = eval(x, globalEnv)
  def evaluate(s: String): Data = evaluate(string2lisp(s))

  def eval(x: Data, env: Environment): Data = x match {
    case _: Int => x
    case Symbol(name) => env.lookup(name)
    case 'quote :: y :: Nil => y
    case 'val :: Symbol(name) :: expr :: rest :: Nil => eval(rest, env.extend(name, eval(expr, env)))
    case 'lambda :: params :: body :: Nil => mkLambda(asList(params) map paramName, body, env)
    case 'and :: x :: y :: Nil => eval('if :: x :: y :: 0 :: Nil, env)
    case 'or :: x :: y :: Nil => eval('if :: x :: 1 :: y :: Nil, env)
    case 'case :: scrut :: (('else :: expr :: Nil) :: rest) => eval(expr, env) 
    case 'who :: Nil => println("Environment:\n" + globalEnv.toString); true
    case 'if :: cond :: thenpart :: elsepart :: Nil => 
      if (eval(cond, env) != 0) eval(thenpart, env)
      else eval(elsepart, env)

    case 'case :: scrut :: ((value :: expr :: Nil) :: rest) => 
      val evaluated = eval(scrut, env)
      if (value == evaluated) eval(expr, env)
      else eval(('case :: evaluated :: Nil) ::: rest, env)

    case 'def :: (name :: args) :: body :: rest :: Nil => 
      eval('def :: name :: List('lambda, args, body) :: rest :: Nil, env)

    case 'def :: (name :: args) :: body :: Nil =>
      eval('def :: name :: List('lambda, args, body) :: Nil, env)

    case 'def :: Symbol(name) :: body :: Nil => // definition GLOBAL
      if (env == globalEnv) {
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
        s"def $name" // just confirm we got the def
      } else {
        sys.error(s"def $name cannot be added to an inner scope because it is global.\n" +
                   "Hint: a local def always has three parameters: a name, a body, and the rest of the code to execute")
      }
    case 'def :: Symbol(name) :: body :: rest :: Nil => // GLOBAL or LOCAL
      if (env == globalEnv)
        globalEnv = env.extendRec(name, env1 => eval(body, env1))
      eval(rest, env.extendRec(name, env1 => eval(body, env1))) // evaluate
    case operator :: operands =>
      try {
        apply(eval(operator, env), operands.map(x => eval(x, env)))
      } catch {
        case ex: MatchError =>
          sys.error(s"bad arguments for function $operator: $operands")
      }
  }
}
