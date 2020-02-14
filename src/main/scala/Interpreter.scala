class Interpreter {
  def interpret(expr: Expr): Unit = {
    try {
      val value: Option[Any] = evaluate(expr)
      println(stringify(value))
    } catch {
      case e: RuntimeError => Lox.error(e)
    }
  }

  private def evaluate(expr: Expr): Option[Any] = expr match {
    case Binary(left, op, right) =>
      val lv    = evaluate(left)
      val rv    = evaluate(right)
      val strip = tok => stripNumber(op, tok)
      op.typ match {
        case TokenType.MINUS => Some(strip(lv) - strip(rv))
        case TokenType.PLUS =>
          (lv, rv) match {
            case (Some(ld: Double), Some(rd: Double)) => Some(ld + rd)
            case (Some(ls: String), Some(rs: String)) => Some(ls + rs)
            case (Some(ls: String), Some(rd: Double)) => Some(ls + rd)
            case _ =>
              val msg =
                "Operands must be pair (number, number), (string, string), or (string, number)."
              throw RuntimeError(op, msg)
          }
        case TokenType.STAR          => Some(strip(lv) * strip(rv))
        case TokenType.SLASH         => Some(strip(lv) / strip(rv))
        case TokenType.GREATER       => Some(strip(lv) > strip(rv))
        case TokenType.GREATER_EQUAL => Some(strip(lv) >= strip(rv))
        case TokenType.LESS          => Some(strip(lv) < strip(rv))
        case TokenType.LESS_EQUAL    => Some(strip(lv) <= strip(rv))
        case TokenType.BANG_EQUAL    => Some(lv != rv)
        case TokenType.EQUAL_EQUAL   => Some(lv == rv)
        case TokenType.COMMA         => rv
      }
    case Grouping(expr) => evaluate(expr)
    case Literal(value) => value
    case Unary(op, right) =>
      val rv = evaluate(right)
      op.typ match {
        case TokenType.MINUS => Some(-stripNumber(op, rv))
        case TokenType.BANG  => Some(!isTruthy(rv))
      }

  }

  private def stripNumber(operator: Token, operand: Option[Any]): Double = operand match {
    case Some(d: Double) => d
    case _               => throw RuntimeError(operator, "Operand must be a number.")
  }

  private def isTruthy(x: Option[Any]): Boolean = x match {
    case None        => false
    case Some(false) => false
    case _           => true
  }

  private def stringify(value: Option[Any]): String = value match {
    case None    => "nil"
    case Some(v) => v.toString
  }
}
