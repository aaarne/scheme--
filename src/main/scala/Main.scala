import scheme.Scheme
import scala.util.{Try, Failure, Success}

object Main {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))

  def main(args: Array[String]): Unit = {
    print("lisp> ")
    in.lines forEach { code => 
      Try(Scheme.evaluate(code)) match {
        case Success(result) => println(Scheme.lisp2string(result))
        case Failure(e) => println(s"Error: $e")
      }
      print("lisp> ")
    }
  }
}
