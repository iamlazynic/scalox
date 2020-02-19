class Interpreter {
  val top = new Environment()

  def interpret(item: Either[Array[Stmt], Expr]): Unit = {
    try {
      item match {
        case Left(statements) => statements.foreach(execute(top))
        case Right(expr)      => println(stringify(evaluate(top)(expr)))
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
      val eval: Expr => Option[Any] = evaluate(env)
      val exec: Stmt => Unit        = execute(env)
      stmt match {
        case Expression(expr) =>
          eval(expr)
        case If(cond, brThen, brElse) =>
          if (isTruthy(eval(cond))) exec(brThen)
          else brElse.foreach(exec)
        case While(cond, body) =>
          while (isTruthy(eval(cond))) exec(body)
        case Print(expr) =>
          println(stringify(eval(expr)))
        case Var(name, initializer) =>
          val value = initializer match { case None => None; case Some(expr) => eval(expr) }
          env.define(name.lexeme, value)
      }
  }

  private def evaluate(env: Environment)(expr: Expr): Option[Any] = expr match {
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
            case TokenType.MINUS => Some(strip(lv) - strip(rv))
            case TokenType.PLUS =>
              (lv, rv) match {
                case (Some(ld: Double), Some(rd: Double)) => Some(ld + rd)
                case (Some(ls: String), Some(rs: String)) => Some(ls + rs)
                case (Some(ls: String), Some(rd: Double)) => Some(ls + rd)
                case _ =>
                  throw RuntimeError(
                    op,
                    "Operands must be pair (number, number), (string, string), or (string, number).")
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
            // TODO: exhausted match?
          }
      }
    case Grouping(expr) => evaluate(env)(expr)
    case Literal(value) => value
    case Unary(op, right) =>
      val rv = evaluate(env)(right)
      op.typ match {
        case TokenType.MINUS => Some(-stripNumber(op, rv))
        case TokenType.BANG  => Some(!isTruthy(rv))
        // TODO: exhausted match?
      }
    case Variable(name) => env.get(name)
  }

  private def stripNumber(operator: Token, operand: Option[Any]): Double = operand match {
    case Some(d: Double) => d
    case _               => throw RuntimeError(operator, "Operand must be a number.")
  }

  private def isTruthy(x: Option[Any]): Boolean = x match {
    case None | Some(false) => false
    case _                  => true
  }

  private def stringify(value: Option[Any]): String = value match {
    case None    => "nil"
    case Some(v) => v.toString
  }
}
