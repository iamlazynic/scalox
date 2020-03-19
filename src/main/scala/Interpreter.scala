import scala.collection.mutable

object Interpreter {
  private case class Return(value: Terminal) extends RuntimeException()
}

class Interpreter {
  private val top      = new Environment()
  private var continue = true
  private val locals   = new mutable.HashMap[Expr, Int]()

  top.define("clock", TNative(0, (_: Vector[Terminal]) => TNumber(System.currentTimeMillis() / 1000.0)))

  def interpret(item: Either[Vector[Stmt], Expr]): Unit = {
    try {
      item match {
        case Left(statements) => statements foreach execute(top)
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
      case Break(_) =>
        continue = false
      case Class(name, superclass, staticMethods, methods) =>
        val parent = superclass.map(variable =>
          evaluate(env)(variable) match {
            case klass: TClass => klass
            case _             => throw RuntimeError(variable.name, "Superclass must be a class.")
        })
        env.define(name.lexeme, TNil())
        val current = new Environment(Some(env))
        parent.foreach(klass => {
          current.define("super", klass)
        })
        val staticMethMap = new mutable.HashMap[String, TFunction]()
        val methMap       = new mutable.HashMap[String, TFunction]()
        for (method <- staticMethods) {
          val (name, params, body) = Function.unapply(method).get
          val func                 = TFunction(params, body, current, isInitializer = false)
          staticMethMap.put(name.lexeme, func)
        }
        for (method <- methods) {
          val (name, params, body) = Function.unapply(method).get
          val func                 = TFunction(params, body, current, name.lexeme == "init")
          methMap.put(name.lexeme, func)
        }
        val klass = TClass(name.lexeme, parent, staticMethMap, methMap, TClass.index)
        env.assign(name, klass)
      case Expression(expr) =>
        eval(expr)
      case Function(name, params, body) =>
        val func = TFunction(params, body, env, isInitializer = false)
        env.define(name.lexeme, func)
      case If(cond, brThen, brElse) =>
        if (isTruthy(eval(cond))) exec(brThen)
        else brElse foreach exec
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

  private def closure(env: Environment)(params: Vector[Token], body: Vector[Stmt])(args: Seq[Terminal]): Terminal = {
    val local = new Environment(Some(env))
    for (i <- params.indices) local.define(params(i).lexeme, args(i))
    try executeSequence(local)(body)
    catch { case returned: Interpreter.Return => return returned.value }
    TNil()
  }

  private def executeSequence(env: Environment)(statements: Vector[Stmt]): Boolean = {
    for (stmt <- statements) {
      execute(env)(stmt)
      if (!continue) return false
    }
    true
  }

  private def evaluate(env: Environment)(expr: Expr): Terminal = expr match {
    case ex @ Assign(name, value, _) =>
      val valu = evaluate(env)(value)
      locals.get(ex) match {
        case None        => top.assign(name, valu)
        case Some(depth) => env.assignAt(depth, name, valu)
      }
      valu
    case Binary(left, op, right, _) =>
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
                  throw RuntimeError(op,
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
    case Call(callee, paren, args, _) =>
      evaluate(env)(callee) match {
        case TNative(arity, func) =>
          if (args.length != arity)
            throw RuntimeError(paren, s"Expected $arity arguments but got ${args.length}.")
          func(args.map(evaluate(env)))
        case TFunction(params, body, clenv, isInitializer) =>
          if (args.length != params.length)
            throw RuntimeError(paren, s"Expected ${params.length} arguments but got ${args.length}.")
          val ret = closure(clenv)(params, body)(args.map(evaluate(env)))
          if (isInitializer) clenv.getAt(0, "this") else ret
        case klass: TClass =>
          val instance = TInstance(klass, TClass.index)
          klass.method("init") match {
            case Some(TFunction(params, body, clenv, _)) =>
              if (args.length != params.length)
                throw RuntimeError(paren, s"Expected ${params.length} arguments but got ${args.length}.")
              closure(envBound(clenv, instance))(params, body)(args.map(evaluate(env)))
            case None =>
              if (args.nonEmpty)
                throw RuntimeError(paren, s"Expected 0 argument but got ${args.length} arguments.")
          }
          instance
        case _ => throw RuntimeError(paren, "Can only call functions and classes.")
      }
    case Get(obj, name, _) =>
      evaluate(env)(obj) match {
        case instance: TInstance =>
          instance.fields.get(name.lexeme) match {
            case Some(property) => property
            case None =>
              instance.klass.method(name.lexeme) match {
                case Some(TFunction(params, body, clenv, isInitializer)) =>
                  TFunction(params, body, envBound(clenv, instance), isInitializer)
                case None =>
                  throw RuntimeError(name, s"Undefined property ${name.lexeme}.")
              }
          }
        case klass: TClass =>
          klass.staticMethod(name.lexeme) match {
            case Some(method) => method
            case None         => throw RuntimeError(name, s"Undefined static method ${name.lexeme}.")
          }
        case _ =>
          throw RuntimeError(name, "Only instances have properties.")
      }
    case Grouping(expr, _) => evaluate(env)(expr)
    case Lambda(params, body, _) =>
      TFunction(params, body, env, isInitializer = false)
    case Literal(value, _) => value
    case Set(obj, name, value, _) =>
      evaluate(env)(obj) match {
        case instance: TInstance =>
          val valu = evaluate(env)(value)
          instance.fields.put(name.lexeme, valu)
          valu
        case _ =>
          throw RuntimeError(name, "Only instances have fields.")
      }
    case ex @ Super(keyword, name, _) =>
      val depth: Int = locals(ex)

      env.getAt(depth, "super") match {
        case parent: TClass =>
          env.getAt(depth - 1, "this") match {
            case instance: TInstance =>
              parent.method(name.lexeme) match {
                case Some(TFunction(params, body, clenv, isInitializer)) =>
                  TFunction(params, body, envBound(clenv, instance), isInitializer)
                case None =>
                  throw RuntimeError(name, s"Undefined property '${name.lexeme}'.")
              }
            case _ => throw new Exception
          }
        case _ => throw new Exception
      }
    case expr: This =>
      lookup(env)(expr.keyword, expr)
    case Unary(op, right, _) =>
      val rv = evaluate(env)(right)
      op.typ match {
        case TokenType.MINUS => TNumber(-stripNumber(op, rv))
        case TokenType.BANG  => TBoolean(!isTruthy(rv))
        // TODO: exhausted match?
      }
    case expr: Variable =>
      lookup(env)(expr.name, expr)
  }

  def resolve(expr: Expr, depth: Int): Unit =
    locals.put(expr, depth)

  private def lookup(env: Environment)(name: Token, expr: Expr): Terminal = locals.get(expr) match {
    case Some(depth) => env.getAt(depth, name.lexeme)
    case None        => top.get(name)
  }

  private def envBound(env: Environment, instance: TInstance): Environment = {
    val local = new Environment(Some(env))
    local.define("this", instance)
    local
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
