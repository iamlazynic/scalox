import TokenType.TokenType

object Parser {
  private class ParsingError(tok: Token, msg: String) extends RuntimeException(msg) {
    def token: Token = tok
  }
}

class Parser(tokens: Array[Token]) {
  private var current = 0

  def parse(repl: Boolean): Either[Array[Stmt], Expr] = {
    var statements = new Array[Stmt](0)
    while (!isAtEnd) {
      try {
        statements = statements :+ declaration
      } catch {
        case err: Parser.ParsingError =>
          if (repl && statements.isEmpty && err.getMessage == "Expect ';' after expression.") {
            current = 0
            return Right(expression)
          }
          Lox.error(err.token, err.getMessage)
          synchronize()
      }
    }
    Left(statements)
  }

  // declaration → var | statement
  private def declaration: Stmt = {
    if (matc(TokenType.VAR)) varDeclaration
    else statement
  }

  // statement → print | if | block | expression
  private def statement: Stmt = {
    if (matc(TokenType.PRINT)) printStmt
    else if (matc(TokenType.IF)) ifStmt
    else if (matc(TokenType.LEFT_BRACE)) blockStmt
    else expressionStmt
  }

  // print → "print" expression ";"
  private def printStmt: Stmt = {
    val value = expression
    consume(TokenType.SEMICOLON, "Expect ';' after value.")
    Print(value)
  }

  // if → "if" "(" expression ")" statement ( "else" statement )?
  private def ifStmt: Stmt = {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
    val cond = expression
    consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
    val brThen = statement
    val brElse = if (matc(TokenType.ELSE)) Some(statement) else None
    If(cond, brThen, brElse)
  }

  private def blockStmt: Stmt = {
    var statements = new Array[Stmt](0)
    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd) {
      try {
        statements = statements :+ declaration
      } catch {
        case err: Parser.ParsingError =>
          Lox.error(err.token, err.getMessage)
          synchronize()
      }
    }
    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
    Block(statements)
  }

  // expression → expression ";"
  private def expressionStmt: Stmt = {
    val expr = expression
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Expression(expr)
  }

  // var → "var" identifier "=" expression
  private def varDeclaration: Stmt = {
    val name: Token = consume(TokenType.IDENTIFIER, "Expect variable name.")
    val initializer: Option[Expr] =
      if (matc(TokenType.EQUAL)) Some(expression)
      else None

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, initializer)
  }

  // expression → assignment
  private def expression: Expr = assignment

  // assignment → identifier "=" assignment | series
  private def assignment: Expr = {
    val expr: Expr = series
    if (matc(TokenType.EQUAL)) {
      val equals: Token = previous
      val value: Expr   = assignment

      expr match {
        case Variable(name) => Assign(name, value)
        case _              => error(equals, "Invalid assignment target."); expr
      }
    } else {
      expr
    }
  }

  // series → logical_or ( "," logical_or )*
  private def series: Expr = {
    val left: Expr = or()
    if (matc(TokenType.COMMA)) {
      val op: Token   = previous
      val right: Expr = series
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

  // unary → ( "!" | "-" ) unary | primary
  private def unary(): Expr = {
    if (matc(TokenType.BANG, TokenType.MINUS)) {
      val op: Token   = previous
      val right: Expr = unary()
      Unary(op, right)
    } else {
      primary()
    }
  }

  // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
  private def primary(): Expr = {
    if (matc(TokenType.FALSE))
      Literal(Some(false))
    else if (matc(TokenType.TRUE))
      Literal(Some(true))
    else if (matc(TokenType.NIL))
      Literal(None)
    else if (matc(TokenType.NUMBER, TokenType.STRING))
      Literal(previous.literal)
    else if (matc(TokenType.IDENTIFIER))
      Variable(previous)
    else if (matc(TokenType.LEFT_PAREN)) {
      val expr: Expr = expression
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
      expression
      throw error(next, s"Binary operator '$symb' expects a left-hand operand.")
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
        case TokenType.CLASS  => return
        case TokenType.FUN    => return
        case TokenType.FOR    => return
        case TokenType.IF     => return
        case TokenType.PRINT  => return
        case TokenType.RETURN => return
        case TokenType.VAR    => return
        case TokenType.WHILE  => return
        case _                =>
      }

      advance()
    }
  }
}
