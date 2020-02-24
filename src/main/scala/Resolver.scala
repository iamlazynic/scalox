import scala.collection.mutable
import Resolver.{FunctionType, FN, NONE}

object Resolver {
  private sealed trait FunctionType
  private case object NONE extends FunctionType
  private case object FN   extends FunctionType
}

class Resolver(interpreter: Interpreter) {
  private var scopes: List[mutable.HashMap[String, Boolean]] = Nil
  private var currentFn: FunctionType                        = NONE
  private var currentLoop: Boolean                           = false

  def resolve(item: Either[Vector[Stmt], Expr]): Unit = item match {
    case Left(statements) => resolve(statements)
    case Right(expr)      => resolve(expr)
  }

  def resolve(statements: Vector[Stmt]): Unit =
    statements foreach resolve

  private def resolve(stmt: Stmt): Unit = stmt match {
    case Block(statements) => beginScope(); resolve(statements); endScope()
    case Break(keyword) =>
      if (!currentLoop) Lox.error(keyword, "Break from loops only.")
    case Expression(expr)             => resolve(expr)
    case Function(name, params, body) => declare(name); define(name); resolveFn(params, body, FN)
    case If(cond, brThen, brElse)     => resolve(cond); resolve(brThen); brElse foreach resolve
    case Print(expr)                  => resolve(expr)
    case Return(keyword, value) =>
      if (currentFn == NONE) Lox.error(keyword, "Return from functions only.")
      value foreach resolve
    case Var(name, initializer) => declare(name); initializer foreach resolve; define(name)
    case While(cond, body) =>
      resolve(cond)
      val enclosingLoop = currentLoop
      currentLoop = true
      resolve(body)
      currentLoop = enclosingLoop
  }

  private def resolve(expr: Expr): Unit = expr match {
    case Assign(name, value)    => resolve(value); resolveLocal(expr, name)
    case Binary(left, _, right) => resolve(left); resolve(right)
    case Call(callee, _, args)  => resolve(callee); args foreach resolve
    case Grouping(expr)         => resolve(expr)
    case Lambda(params, body)   => resolveFn(params, body, FN)
    case Literal(_)             =>
    case Unary(_, right)        => resolve(right)
    case Variable(name) =>
      scopes match {
        case Nil =>
        case scope :: _ =>
          if (scope.get(name.lexeme).contains(false))
            Lox.error(name, "Cannot read local variable in its own initializer.")
      }
      resolveLocal(expr, name)
  }

  private def resolveFn(params: Vector[Token], body: Vector[Stmt], fnType: FunctionType): Unit = {
    val enclosingFn = currentFn
    currentFn = fnType
    beginScope()
    for (param <- params) {
      declare(param)
      define(param)
    }
    resolve(body)
    endScope()
    currentFn = enclosingFn
  }

  @scala.annotation.tailrec
  private def resolveLocal(expr: Expr,
                           name: Token,
                           scopes: List[mutable.HashMap[String, Boolean]] = scopes,
                           depth: Int = 0): Unit = scopes match {
    case Nil =>
    case scope :: next =>
      if (scope.contains(name.lexeme)) interpreter.resolve(expr, depth)
      else resolveLocal(expr, name, next, depth + 1)
  }

  private def beginScope(): Unit =
    scopes = mutable.HashMap[String, Boolean]() :: scopes

  private def endScope(): Unit =
    scopes = scopes.tail

  private def declare(name: Token): Unit = scopes match {
    case Nil =>
    case scope :: _ =>
      if (scope.contains(name.lexeme))
        Lox.error(name, "Variable with this name already declared in this scope.")
      scope.put(name.lexeme, false)
  }

  private def define(name: Token): Unit = scopes match {
    case Nil        =>
    case scope :: _ => scope.put(name.lexeme, true)
  }
}
