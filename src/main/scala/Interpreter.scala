class Interpreter {
  val top = new Environment()

  def interpret(item: Either[Array[Stmt], Expr]): Unit = {
    try {
      item match {
        case Left(statements) => statements.foreach(execute(top))
        case Right(expr)      => println(evaluate(top)(expr))
      }
    } catch {
      case e: RuntimeError => Lox.error(e)
    }
  }

  private def execute(env: Environment)(stmt: Stmt): Unit = stmt match {
    case Block(statements) =>
      val local = new Environment(Some(env))
      statements.foreach(execute(local))
    case stmt =>
      val eval: Expr => Terminal = evaluate(env)
      val exec: Stmt => Unit     = execute(env)
      stmt match {
        case Expression(expr) =>
          eval(expr)
        case If(cond, brThen, brElse) =>
          if (isTruthy(eval(cond))) exec(brThen)
          else brElse.foreach(exec)
        case While(cond, body) =>
          while (isTruthy(eval(cond))) exec(body)
        case Print(expr) =>
          println(eval(expr))
        case Var(name, initializer) =>
          val value = initializer match { case None => TNil(); case Some(expr) => eval(expr) }
          env.define(name.lexeme, value)
      }
  }

  private def evaluate(env: Environment)(expr: Expr): Terminal = expr match {
    case Assign(name, value) =>
      val valu = evaluate(env)(value)
      env.assign(name, valu)
      valu
    case Binary(left, op, right) =>
      val lv = evaluate(env)(left)
      op.typ match {
        case TokenType.OR  => if (isTruthy(lv)) lv else evaluate(env)(right)
        case TokenType.AND => if (!isTruthy(lv)) lv else evaluate(env)(right)
        case typ =>
          val rv    = evaluate(env)(right)
          val strip = tok => stripNumber(op, tok)
          typ match {
            case TokenType.MINUS => TNumber(strip(lv) - strip(rv))
            case TokenType.PLUS =>
              (lv, rv) match {
                case (TNumber(ld: Double), TNumber(rd: Double)) => TNumber(ld + rd)
                case (TString(ls: String), TString(rs: String)) => TString(ls + rs)
                case (TString(ls: String), TNumber(rd: Double)) => TString(ls + rd)
                case _ =>
                  throw RuntimeError(
                    op,
                    "Operands must be pair (number, number), (string, string), or (string, number).")
              }
            case TokenType.STAR          => TNumber(strip(lv) * strip(rv))
            case TokenType.SLASH         => TNumber(strip(lv) / strip(rv))
            case TokenType.GREATER       => TBoolean(strip(lv) > strip(rv))
            case TokenType.GREATER_EQUAL => TBoolean(strip(lv) >= strip(rv))
            case TokenType.LESS          => TBoolean(strip(lv) < strip(rv))
            case TokenType.LESS_EQUAL    => TBoolean(strip(lv) <= strip(rv))
            case TokenType.BANG_EQUAL    => TBoolean(lv != rv)
            case TokenType.EQUAL_EQUAL   => TBoolean(lv == rv)
            case TokenType.COMMA         => rv
            // TODO: exhausted match?
          }
      }
    case Grouping(expr) => evaluate(env)(expr)
    case Literal(value) => value
    case Unary(op, right) =>
      val rv = evaluate(env)(right)
      op.typ match {
        case TokenType.MINUS => TNumber(-stripNumber(op, rv))
        case TokenType.BANG  => TBoolean(!isTruthy(rv))
        // TODO: exhausted match?
      }
    case Variable(name) => env.get(name)
  }

  private def stripNumber(operator: Token, operand: Terminal): Double = operand match {
    case TNumber(d) => d
    case _          => throw RuntimeError(operator, "Operand must be a number.")
  }

  private def isTruthy(x: Terminal): Boolean = x match {
    case TNil() | TBoolean(false) => false
    case _                        => true
  }
}
