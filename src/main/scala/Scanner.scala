import TokenType.TokenType

import scala.collection.immutable.HashMap

object Scanner {
  private val keywords = HashMap[String, TokenType](
    ("and", TokenType.AND),
    ("break", TokenType.BREAK),
    ("class", TokenType.CLASS),
    ("else", TokenType.ELSE),
    ("false", TokenType.FALSE),
    ("for", TokenType.FOR),
    ("fun", TokenType.FUN),
    ("if", TokenType.IF),
    ("nil", TokenType.NIL),
    ("or", TokenType.OR),
    ("print", TokenType.PRINT),
    ("return", TokenType.RETURN),
    ("super", TokenType.SUPER),
    ("this", TokenType.THIS),
    ("true", TokenType.TRUE),
    ("var", TokenType.VAR),
    ("while", TokenType.WHILE),
  )

  // https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
  private val escapes = HashMap[Char, Byte](
    ('a', 0x07),
    ('b', 0x08),
    ('e', 0x1b),
    ('f', 0x0c),
    ('n', 0x0a),
    ('r', 0x0d),
    ('t', 0x09),
    ('v', 0x0b),
  )
}

class Scanner(private val source: String) {
  private var tokens  = new Array[Token](0)
  private var start   = 0
  private var current = 0
  private var line    = 1

  def scan(): Array[Token] = {
    while (!isAtEnd) {
      // Are at the beginning of the next lexeme
      start = current
      scanToken()
    }

    val eof: Token = Token(TokenType.EOF, "", None, line)
    tokens = tokens :+ eof
    tokens
  }

  private def scanToken(): Unit = {
    advance() match {
      // Single chars that can be determined right away
      case '(' => addToken(TokenType.LEFT_PAREN)
      case ')' => addToken(TokenType.RIGHT_PAREN)
      case '{' => addToken(TokenType.LEFT_BRACE)
      case '}' => addToken(TokenType.RIGHT_BRACE)
      case ',' => addToken(TokenType.COMMA)
      case '.' => addToken(TokenType.DOT)
      case '-' => addToken(TokenType.MINUS)
      case '+' => addToken(TokenType.PLUS)
      case ';' => addToken(TokenType.SEMICOLON)
      case '*' => addToken(TokenType.STAR)

      // Either 1-char or the first of a 2-char lexeme
      case '!' =>
        addToken(if (matc('=')) TokenType.BANG_EQUAL else TokenType.BANG)
      case '=' =>
        addToken(if (matc('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL)
      case '<' =>
        addToken(if (matc('=')) TokenType.LESS_EQUAL else TokenType.LESS)
      case '>' =>
        addToken(if (matc('=')) TokenType.GREATER_EQUAL else TokenType.GREATER)

      // Slash
      case '/' =>
        if (matc('/')) {
          while (!next.contains('\n') && !isAtEnd) advance()
        } else {
          addToken(TokenType.SLASH)
        }

      // White space
      case ' '  =>
      case '\r' =>
      case '\t' =>
      case '\n' => line += 1

      // String literals
      case '"' => scanString()

      case c =>
        // Number literals
        if (c.isDigit) scanNumber()
        // Identifiers
        else if (isAlpha(Some(c))) scanIdentifier()
        // Unexpected character
        else Lox.error(line, s"Unexpected character $c.")
    }
  }

  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  private def matc(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source.charAt(current) != expected) return false

    current += 1
    true
  }

  private def next: Option[Char] =
    if (isAtEnd) None else Some(source.charAt(current))

  private def nexnext: Option[Char] =
    if (current + 1 >= source.length) None else Some(source.charAt(current + 1))

  private def isDigit(c: Option[Char]): Boolean = {
    c match {
      case None    => false
      case Some(c) => c.isDigit
    }
  }

  private def isAlpha(c: Option[Char]): Boolean = {
    c match {
      case None => false
      case Some(c) =>
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')
    }
  }

  private def addToken(typ: TokenType, literal: Option[Terminal] = None): Unit = {
    val text: String = source.substring(start, current)
    val tok: Token   = Token(typ, text, literal, line)
    tokens = tokens :+ tok
  }

  private def scanString(): Unit = {
    var value: String     = ""
    var continue: Boolean = true

    while (continue) {
      next match {
        case None => continue = false
        case Some('\\') =>
          advance()
          next match {
            case None => continue = false
            case Some('\n') =>
              line += 1
              advance()
            case Some(c) =>
              advance()
              val newc = Scanner.escapes.get(c) match {
                case None       => c
                case Some(byte) => byte.toChar
              }
              value += newc
          }
        case Some('"') =>
          continue = false
        case Some(c) =>
          advance()
          if (c == '\n') line += 1
          value += c
      }
    }

    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
      return
    }

    // Consume the closing '"'
    advance()

    addToken(TokenType.STRING, Some(TString(value)))
  }

  private def scanNumber(): Unit = {
    while (isDigit(next)) advance()

    if (next.contains('.') && isDigit(nexnext)) {
      do advance() while (isDigit(next))
    }

    val value = source.substring(start, current).toDouble
    addToken(TokenType.NUMBER, Some(TNumber(value)))
  }

  private def scanIdentifier(): Unit = {
    while (isDigit(next) || isAlpha(next)) advance()

    // See if it is a reserved word (keyword)
    val text: String = source.substring(start, current)
    Scanner.keywords.get(text) match {
      case None      => addToken(TokenType.IDENTIFIER)
      case Some(typ) => addToken(typ)
    }
  }

  private def isAtEnd: Boolean =
    current >= source.length
}
