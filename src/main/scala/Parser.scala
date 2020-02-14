import TokenType.TokenType

object Parser {
  private class ParsingException extends RuntimeException
}

class Parser(tokens: Array[Token]) {
  private var current = 0

  def parse(): Option[Expr] = {
    try {
      Some(expression)
    } catch {
      case _: Parser.ParsingException => None
    }
  }

  def expression: Expr = equality

  // equality → comparison ( ( "!=" | "==" ) comparison )*
  private def equality: Expr = {
    // left
    var expr: Expr = comparison
    // ( ... )*
    while (matc(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
      val op: Token   = previous
      val right: Expr = comparison
      expr = Binary(expr, op, right)
    }
    expr
  }

  // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
  private def comparison: Expr = {
    // left
    var expr: Expr = addition
    // ( ... )*
    while (matc(
             TokenType.GREATER,
             TokenType.GREATER_EQUAL,
             TokenType.LESS,
             TokenType.LESS_EQUAL
           )) {
      val op: Token   = previous
      val right: Expr = addition
      expr = Binary(expr, op, right)
    }
    expr
  }

  // addition → multiplication ( ( "-" | "+" ) multiplication )*
  private def addition: Expr = {
    // left
    var expr: Expr = multiplication
    // ( ... )*
    while (matc(TokenType.PLUS, TokenType.MINUS)) {
      val op: Token   = previous
      val right: Expr = multiplication
      expr = Binary(expr, op, right)
    }
    expr
  }

  // multiplication → unary ( ( "/" | "*" ) unary )*
  private def multiplication: Expr = {
    // left
    var expr: Expr = unary
    // ( ... )*
    while (matc(TokenType.SLASH, TokenType.STAR)) {
      val op: Token   = previous
      val right: Expr = unary
      expr = Binary(expr, op, right)
    }
    expr
  }

  // unary → ( "!" | "-" ) unary | primary
  private def unary: Expr = {
    if (matc(TokenType.BANG, TokenType.MINUS)) {
      val op: Token   = previous
      val right: Expr = unary
      Unary(op, right)
    } else {
      primary
    }
  }

  // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
  private def primary: Expr = {
    if (matc(TokenType.FALSE)) Literal(Some(false))
    else if (matc(TokenType.TRUE)) Literal(Some(true))
    else if (matc(TokenType.NIL)) Literal(None)
    else if (matc(TokenType.NUMBER, TokenType.STRING))
      Literal(previous.literal)
    else if (matc(TokenType.LEFT_PAREN)) {
      val expr: Expr = expression
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    } else throw error(next, "Expect expression.")
  }

  private def matc(types: TokenType*): Boolean = {
    types.find(check) match {
      case None    => false
      case Some(_) => advance(); true
    }
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

  private def consume(typ: TokenType, message: String): Unit = {
    if (check(typ)) advance()
    else throw error(next, message)
  }

  private def error(token: Token, message: String): Parser.ParsingException = {
    Lox.error(token, message)
    new Parser.ParsingException
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
      }

      advance()
    }
  }
}
