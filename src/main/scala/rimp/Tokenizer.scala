package rimp

class Tokenizer extends Lexer {

  // ----------- RIMP.Tokenizer ------------

  abstract class Token
  case object T_SEMI extends Token
  case object T_COMMA extends Token
  case object T_LPAREN extends Token
  case object T_RSQRB extends Token
  case object T_LSQRB extends Token
  case object T_RPAREN extends Token
  case object T_LBRACK extends Token
  case object T_RBRACK extends Token
  case object T_BAR extends Token
  case class T_ID(s: String) extends Token
  case class T_OP(s: String) extends Token
  case class T_NUM(n: Int) extends Token
  case class T_KWD(s: String) extends Token
  case class T_STR(s: String) extends Token

  val token: PartialFunction[(String, String), Token] = {
    case ("semicolon", _) => T_SEMI
    case ("comma", _) => T_COMMA
    case ("parenthesis", "{") => T_LPAREN
    case ("parenthesis", "}") => T_RPAREN
    case ("brackets", "(") => T_LBRACK
    case ("brackets", ")") => T_RBRACK
    case ("sqr_brackets", "[") => T_LSQRB
    case ("sqr_brackets", "]") => T_RSQRB
    case ("bar", "|") => T_BAR
    case ("id", s) => T_ID(s)
    case ("operation", s) => T_OP(s)
    case ("number", s) => T_NUM(s.toInt)
    case ("keyword", s) => T_KWD(s)
    case ("string", s) => T_STR(s)
  }

  // by using collect we filter out all unwanted tokens
  def tokenize(s: String): List[Token] =
   lexing_simp(RIMP_REGS, s).collect(token)
}
