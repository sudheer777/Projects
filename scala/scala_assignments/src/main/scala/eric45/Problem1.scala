package eric45

import scala.io._

object Problem1 {
  case class UserError(gripe: String) extends Exception(gripe)

  abstract class Console {
    var verbose = false // print stack traces if true
    // override in an extension
    def execute(cmmd: String): String

    def repl: Unit = {
      var more = true
      var text = ""
      while (more) {
        text = StdIn.readLine("->")
        if (text == "exit") {
          println("Session Ended")
          more = false
        } else if (text == "quit") {
          println("bye")
          more = false
        }
        else
          try
            println(execute(text))
          catch {
            case e: UserError =>
              println(e.getMessage)
              if (verbose)
                e.printStackTrace()
          }

      }
    }
  }

  import scala.collection.mutable

  trait Learner {
    def tell(query: String, answer: String) = "Got it!"
  }

  trait Oracle extends Learner {
    val dunno = "Sorry, I don't know"
    var debug = true

    def ask(query: String): String = {
      if (debug) println("asking oracle")
      dunno
    }
  }

  object QueryConsole extends Console with MathOracle with GeoOracle with BioOracle with MagicOracle with App {
    override def execute(query: String): String = {
      val eqPos = query.indexOf("=")
      var isFact = ( eqPos != -1)
      if (isFact) {
        tell(query.substring(0, eqPos).trim, query.substring(eqPos + 1).trim)
      } else {
        ask(query.trim)
      }
    }
    repl
  }

  trait MathOracle extends Oracle {
    override def ask(query: String): String = {
      if (debug) println("asking math oracle")
      try {
        eval(query)
      } catch {
        case _: Exception => super.ask(query)
      }
    }

    private def eval(query: String): String = {
      val tokens = query.split("\\s+")
      if (tokens.length <= 1) throw UserError("Must have at least one argument")
      val args = tokens.drop(1).map(_.toDouble) // might throw here too
      var result = 0.0
      tokens(0) match {
        case "add" => for (arg <- args) result += arg; result.toString
        case "sub" => for (arg <- args) result -= arg; result.toString
        case "mul" => result = 1; for (arg <- args) result *= arg; result.toString
        case "div" => result = 1; for (arg <- args) result /= arg; result.toString
        case _ => super.ask(query)
      }
    }
  }

  trait MagicOracle extends Oracle {
    val randomAnswerSet = Set("Ask later", "Doubtful", "Maybe", "Probably", "Possible")
    private def randomItem[T](items: Traversable[T]): T = {
      val i = scala.util.Random.nextInt(items.size)
      items.view(i, i + 1).head
    }

    override def ask(query: String): String = {
      if (debug) println("asking magic oracle")
      if (query.endsWith("?")) {
        randomItem(randomAnswerSet)
      } else {
        super.ask(query)
      }
    }
  }

  trait KbaseOracle extends Oracle {
    val kbase = mutable.Map[String, String]()
    override def ask(query: String): String = {
      if (debug) println("asking kbase oracle")
      kbase.getOrElse(query, super.ask(query))
    }

    override def tell(query: String, answer: String): String = {
      kbase(query) = answer
      "Got it!"
    }
  }

  trait GeoOracle extends KbaseOracle {
    kbase("capitol of CA") = "Sacramento"
    kbase("capitol of VA") = "Richmond"
    kbase("capitol of NY") = "Albany"
    kbase("capitol of TX") = "Austin"
  }

  trait BioOracle extends KbaseOracle {
    kbase("giraffe is a") = "mammal"
    kbase("ant is a") = "insect"
    kbase("snake is a") = "reptile"
    kbase("tuna is a") = "fish"
  }
}