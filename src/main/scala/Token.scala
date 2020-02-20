import TokenType.TokenType

case class Token(typ: TokenType, lexeme: String, literal: Option[Terminal], line: Int) {
  override def toString: String = {
    s"$typ $lexeme $literal"
  }
}
