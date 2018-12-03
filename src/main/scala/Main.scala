import scheme.{Interpreter, Lexer}
import scala.util.{Try, Failure, Success}

object Main {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))

  def main(args: Array[String]): Unit = {
    print("lisp> ")
    in.lines forEach { code => 
      Try(Interpreter.evaluate(code)) match {
        case Success(result) => println(Interpreter.lisp2string(result))
        case Failure(e) => println(s"Error: $e")
      }
      print("lisp> ")
    }
  }
}
