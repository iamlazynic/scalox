import TokenType.TokenType

import scala.reflect.ClassTag

object Parser {
  private case class ParsingError(token: Token, msg: String) extends RuntimeException(msg)
}

class Parser(tokens: Array[Token]) {
  private var current = 0

  def parse(repl: Boolean): Either[Array[Stmt], Expr] = {
    var statements = new Array[Stmt](0)
    while (!isAtEnd) {
      try {
        statements = statements :+ declaration(loop = false)
      } catch {
        case err: Parser.ParsingError =>
          if (repl && statements.isEmpty && err.getMessage == "Expect ';' after expression.") {
            current = 0
            return Right(expression())
          }
          Lox.error(err.token, err.getMessage)
          synchronize()
      }
    }
    Left(statements)
  }

  // declaration → var | function | statement
  private def declaration(loop: Boolean): Stmt = {
    if (matc(TokenType.VAR)) return varDeclaration()
    if (matc(TokenType.FUN))
      if (next.typ == TokenType.IDENTIFIER) return function("function")
      else current = current - 1 // FIXME: stepping back, terrible?
    statement(loop)
  }

  // function → "fun" IDENTIFIER "(" parameters? ")" block
  private def function(kind: String): Stmt = {
    val name: Token = consume(TokenType.IDENTIFIER, s"Expect $kind name.")
    consume(TokenType.LEFT_PAREN, s"Expect '(' after $kind name.")
    val params: Array[Token] =
      patternCommaSep("parameters", () => consume(TokenType.IDENTIFIER, "Expect parameter name."))
    consume(TokenType.LEFT_BRACE, s"Expect '{' before $kind body.")
    Function(name, params, block(false))
  }

  // statement → print | if | while | for | block | return | expression
  private def statement(loop: Boolean): Stmt = {
    if (matc(TokenType.PRINT)) printStmt()
    else if (matc(TokenType.IF)) ifStmt(loop)
    else if (matc(TokenType.WHILE)) whileStmt()
    else if (matc(TokenType.FOR)) forStmt()
    else if (matc(TokenType.LEFT_BRACE)) Block(block(loop))
    else if (loop && matc(TokenType.BREAK)) breakStmt()
    else if (matc(TokenType.RETURN)) returnStmt()
    else expressionStmt()
  }

  // print → "print" expression ";"
  private def printStmt(): Stmt = {
    val value = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    Print(value)
  }

  // if → "if" "(" expression ")" statement ( "else" statement )?
  private def ifStmt(loop: Boolean): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val cond = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
    val brThen = statement(loop)
    val brElse = if (matc(TokenType.ELSE)) Some(statement(loop)) else None
    If(cond, brThen, brElse)
  }

  // whileStmt → "while" "(" expression ")" statement
  private def whileStmt(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
    val cond = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.")
    val body = statement(loop = true)
    While(cond, body)
  }

  // forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
  private def forStmt(): Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer: Option[Stmt] =
      if (matc(TokenType.SEMICOLON)) None
      else if (matc(TokenType.VAR)) Some(varDeclaration())
      else Some(expressionStmt())
    val cond: Option[Expr] =
      if (check(TokenType.SEMICOLON)) None
      else Some(expression())
    consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")
    val increment: Option[Expr] =
      if (check(TokenType.RIGHT_PAREN)) None
      else Some(expression())
    consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
    var body = statement(loop = true)

    // desugaring
    increment.foreach(expr => body = Block(Array(body, Expression(expr))))
    body = While(cond.getOrElse(Literal(TBoolean(true))), body)
    initializer.foreach(stmt => body = Block(Array(stmt, body)))

    body
  }

  private def block(loop: Boolean): Array[Stmt] = {
    var statements = new Array[Stmt](0)
    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd) {
      try {
        statements = statements :+ declaration(loop)
      } catch {
        case err: Parser.ParsingError =>
          Lox.error(err.token, err.getMessage)
          synchronize()
      }
    }
    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
    statements
  }

  // break → "break" ";"
  private def breakStmt(): Stmt = {
    consume(TokenType.SEMICOLON, "Expect ';' after 'break'.")
    Break()
  }

  // returnStmt → "return" expression? ";"
  private def returnStmt(): Stmt = {
    val keyword: Token      = previous
    val value: Option[Expr] = if (check(TokenType.SEMICOLON)) None else Some(expression())
    consume(TokenType.SEMICOLON, "Expect ';' after return value.")
    Return(keyword, value)
  }

  // expression → expression ";"
  private def expressionStmt(): Stmt = {
    val expr = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  // var → "var" identifier "=" expression
  private def varDeclaration(): Stmt = {
    val name: Token = consume(TokenType.IDENTIFIER, "Expect variable name.")
    val initializer: Option[Expr] =
      if (matc(TokenType.EQUAL)) Some(expression())
      else None

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, initializer)
  }

  // expression → assignment
  private def expression(): Expr = assignment()

  // assignment → identifier "=" assignment | series
  private def assignment(): Expr = {
    val expr: Expr = series()
    if (matc(TokenType.EQUAL)) {
      val equals: Token = previous
      val value: Expr   = assignment()

      expr match {
        case Variable(name) => Assign(name, value)
        case _              => error(equals, "Invalid assignment target."); expr
      }
    } else {
      expr
    }
  }

  // series → logical_or ( "," logical_or )*
  private def series(): Expr = {
    val left: Expr = or()
    if (matc(TokenType.COMMA)) {
      val op: Token   = previous
      val right: Expr = series()
      Binary(left, op, right)
    } else {
      left
    }
  }

  // logical_or → logic_and ( "or" logic_and )*
  private def or(): Expr =
    patternAxALeft(and, TokenType.OR)

  // logical_and → equality ( "or" equality )*
  private def and(): Expr =
    patternAxALeft(equality, TokenType.AND)

  // equality → comparison ( ( "!=" | "==" ) comparison )*
  private def equality(): Expr =
    patternAxALeft(comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)

  // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
  private def comparison(): Expr =
    patternAxALeft(addition,
                   TokenType.GREATER,
                   TokenType.GREATER_EQUAL,
                   TokenType.LESS,
                   TokenType.LESS_EQUAL)

  // addition → multiplication ( ( "-" | "+" ) multiplication )*
  private def addition(): Expr =
    patternAxALeft(multiplication, TokenType.PLUS, TokenType.MINUS)

  // multiplication → unary ( ( "/" | "*" ) unary )*
  private def multiplication(): Expr =
    patternAxALeft(unary, TokenType.SLASH, TokenType.STAR)

  // unary → ( "!" | "-" ) unary | lambda
  private def unary(): Expr = {
    if (matc(TokenType.BANG, TokenType.MINUS)) {
      val op: Token   = previous
      val right: Expr = unary()
      Unary(op, right)
    } else {
      lambda()
    }
  }

  // lambda → "fun" "(" parameters? ")" block | call
  private def lambda(): Expr = {
    if (matc(TokenType.FUN)) {
      consume(TokenType.LEFT_PAREN, s"Expect '(' after fun in a lambda.")
      val params: Array[Token] =
        patternCommaSep("parameters", () => consume(TokenType.IDENTIFIER, "Expect parameter name."))
      consume(TokenType.LEFT_BRACE, s"Expect '{' before lambda body.")
      Lambda(params, block(false))
    } else {
      call()
    }
  }

  // call      → primary ( "(" arguments? ")" )*
  // arguments → expression ( "," expression )*
  private def call(): Expr = {
    var expr: Expr = primary()
    while (true) {
      if (matc(TokenType.LEFT_PAREN)) {
        val args: Array[Expr] = patternCommaSep("arguments", expression)
        expr = Call(expr, previous, args)
      } else {
        return expr
      }
    }
    expr
  }

  private def patternCommaSep[T](itemKind: String, eat: () => T)(
      implicit m: ClassTag[T]): Array[T] = {
    var items = new Array[T](0)
    if (!check(TokenType.RIGHT_PAREN)) {
      do {
        if (items.length >= 255) Lox.error(next, s"Cannot have more than 255 $itemKind.")
        items = items :+ eat()
      } while (matc(TokenType.COMMA))
    }
    consume(TokenType.RIGHT_PAREN, s"Expect ')' after $itemKind.")
    items
  }

  // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
  private def primary(): Expr = {
    if (matc(TokenType.FALSE))
      Literal(TBoolean(false))
    else if (matc(TokenType.TRUE))
      Literal(TBoolean(true))
    else if (matc(TokenType.NIL))
      Literal(TNil())
    else if (matc(TokenType.NUMBER, TokenType.STRING))
      Literal(previous.literal.get)
    else if (matc(TokenType.IDENTIFIER))
      Variable(previous)
    else if (matc(TokenType.LEFT_PAREN)) {
      val expr: Expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    } else if (matc(
                 TokenType.COMMA,
                 TokenType.BANG_EQUAL,
                 TokenType.EQUAL_EQUAL,
                 TokenType.GREATER,
                 TokenType.GREATER_EQUAL,
                 TokenType.LESS,
                 TokenType.LESS_EQUAL,
                 TokenType.PLUS,
                 TokenType.MINUS,
                 TokenType.SLASH,
                 TokenType.STAR
               )) {
      val symb = previous.lexeme
      expression()
      throw error(next, s"Binary operator '$symb' expects a left-hand operand.")
    } else if (matc(TokenType.BREAK)) {
      throw error(next, "Break statements are not expected outside loops.")
    } else {
      throw error(next, "Expect expression.")
    }
  }

  private def patternAxALeft(A: () => Expr, x: TokenType*): Expr = {
    // left
    var expr: Expr = A()
    // ( ... )*
    while (matc(x: _*)) {
      val op: Token   = previous
      val right: Expr = A()
      expr = Binary(expr, op, right)
    }
    expr
  }

  private def matc(types: TokenType*): Boolean = types.find(check) match {
    case None    => false
    case Some(_) => advance(); true
  }

  private def check(typ: TokenType): Boolean =
    !isAtEnd && next.typ == typ

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }

  private def isAtEnd: Boolean =
    next.typ == TokenType.EOF

  private def next: Token =
    tokens(current)

  private def previous: Token =
    tokens(current - 1)

  private def consume(typ: TokenType, message: String): Token = {
    if (check(typ)) advance()
    else throw error(next, message)
  }

  private def error(token: Token, message: String): Parser.ParsingError = {
    new Parser.ParsingError(token, message)
  }

  private def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      if (previous.typ == TokenType.SEMICOLON) return

      next.typ match {
        case TokenType.CLASS | TokenType.FUN | TokenType.FOR | TokenType.IF | TokenType.PRINT |
            TokenType.RETURN | TokenType.VAR | TokenType.WHILE =>
          return
        case _ =>
      }

      advance()
    }
  }
}
