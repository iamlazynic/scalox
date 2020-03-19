object TokenType extends Enumeration {
  type TokenType = Value

  val LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH,
  STAR, // Single-character tokens.
  BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, // One or two character tokens.
  IDENTIFIER, STRING, NUMBER, // Literals.
  AND, BREAK, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, // Keywords.
  EOF // EOF
  = Value
}
