import scala.io.{Source, StdIn}

object Lox {
  private val interpreter: Interpreter = new Interpreter
  private var hadError = false
  private var hadRuntimeError = false

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: scalox [script]")
      sys.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val buffered     = Source.fromFile(path)
    val text: String = buffered.getLines.mkString
    buffered.close
    run(text)

    // Indicate an error in the exit code
    if (hadError) sys.exit(65)
    if (hadRuntimeError) sys.exit(70)
  }

  private def runPrompt(): Unit = {
    while (true) {
      printf("> ")
      val text = StdIn.readLine
      run(text)

      hadError = false
    }
  }

  private def run(source: String): Unit = {
    val scanner               = new Scanner(source)
    val tokens: Array[Token]  = scanner.scan()
    val parser                = new Parser(tokens)
    val optExpr: Option[Expr] = parser.parse()

    // Stop if there's any syntax error
    optExpr match {
      case None       =>
      case Some(expr) => interpreter.interpret(expr)
    }
  }

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def error(tok: Token, message: String): Unit = {
    if (tok.typ == TokenType.EOF) report(tok.line, " at end", message)
    else report(tok.line, s" at '${tok.lexeme}'", message)
  }

  def error(err: RuntimeError): Unit = {
    Console.err.println(s"${err.message}\n[line ${err.token.line}]")
    hadRuntimeError = true
  }

  // Separate code that generates and reports errors
  private def report(line: Int, where: String, message: String): Unit = {
    Console.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }
}
