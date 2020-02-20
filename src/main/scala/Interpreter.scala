object Interpreter {
  case class Return(value: Terminal) extends RuntimeException()
}

class Interpreter {
  private val top      = new Environment()
  private var continue = true

  top.define("clock",
             TFunction(0, (_: Seq[Terminal]) => TNumber(System.currentTimeMillis() / 1000.0)))

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

  private def execute(env: Environment)(stmt: Stmt): Unit = {
    val eval: Expr => Terminal = evaluate(env)
    val exec: Stmt => Unit     = execute(env)
    stmt match {
      case Block(statements) =>
        val local = new Environment(Some(env))
        continue = executeSequence(local)(statements)
      case Break() =>
        continue = false
      case Expression(expr) =>
        eval(expr)
      case Function(name, params, body) =>
        def call(args: Seq[Terminal]): Terminal = {
          val local = new Environment(Some(env))
          for (i <- params.indices) local.define(params(i).lexeme, args(i))
          try executeSequence(local)(body)
          catch { case returned: Interpreter.Return => return returned.value }
          TNil()
        }
        env.define(name.lexeme, TFunction(params.length, call))
      case If(cond, brThen, brElse) =>
        if (isTruthy(eval(cond))) exec(brThen)
        else brElse.foreach(exec)
      case While(cond, body) =>
        while (continue && isTruthy(eval(cond))) exec(body)
        continue = true
      case Print(expr) =>
        println(eval(expr))
      case Return(_, value) =>
        throw Interpreter.Return(value match { case Some(v) => eval(v); case None => TNil() })
      case Var(name, initializer) =>
        val value = initializer match { case None => TNil(); case Some(expr) => eval(expr) }
        env.define(name.lexeme, value)
    }
  }

  private def executeSequence(env: Environment)(statements: Array[Stmt]): Boolean = {
    for (stmt <- statements) {
      execute(env)(stmt)
      if (!continue) return false
    }
    true
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
                case (TNumber(ld), TNumber(rd)) => TNumber(ld + rd)
                case (TString(ls), TString(rs)) => TString(ls + rs)
                case (l: TString, r: TNumber)   => TString(l.toString + r.toString)
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
    case Call(callee, paren, args) =>
      evaluate(env)(callee) match {
        case TFunction(arity, func) =>
          if (args.length != arity)
            throw RuntimeError(paren, s"Expected $arity arguments but got ${args.length}.")
          func(args.map(evaluate(env)))
        case _ => throw RuntimeError(paren, "Can only call functions and classes.")
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
