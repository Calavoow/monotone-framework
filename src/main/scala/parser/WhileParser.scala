package parser

import scala.util.parsing.combinator.RegexParsers

object WhileParser extends RegexParsers {

	override type Elem = Char

	def identifier = """[a-zA-Z][a-zA-Z0-9]*""".r
	def integer = """(0|[1-9]\d*)""".r ^^ {_.toInt}

	def op_a = """[\-\+\*]""".r ^^ {_.toString}
	def op_r = """<|>|==""".r ^^ {_.toString}
	def op_b = """&&|\|\|""".r ^^ {_.toString}

	def aExp: Parser[AExp] = term ~ (op_a ~ term).* ^^ mkTreeArithmatic

	def term : Parser[AExp] = (
		("(" ~> aExp <~ ")")
			| (identifier ^^ Ref)
			| (integer ^^ INT)
	)

	def mkTreeArithmatic(input: AExp ~ List[String ~ AExp]): AExp = {
		def combine(acc: AExp, next: String ~ AExp) = {
			next match {
				case op ~ y => BinOp(op, acc, y)
			}
		}

		input match {
			case first ~ rest => ((first: AExp) /: rest)(combine)
		}
	}

	def bExp: Parser[BExp] = (
		binTerm ~ (op_b ~ binTerm).* ^^ mkTreeBoolean
			| "not" ~> bExp ^^ {case e => Not(e)}
	)

	def binTerm = (
		"true" ^^ {case _ => True}
			| "false" ^^ {case _ => False}
			| aExp ~ op_r ~ aExp ^^ { case e1 ~ op ~ e2 => RelationalExp(op, e1, e2)}
	)

	def mkTreeBoolean(input: BExp ~ List[String ~ BExp]): BExp = {
		def combine(acc: BExp, next: String ~ BExp) = {
			next match {
				case op ~ y => BinBExp(op, acc, y)
			}
		}

		input match {
			case first ~ rest => ((first: BExp) /: rest)(combine)
		}
	}


	def statements = statement.*
	def block = "{" ~> statements <~ "}" ^^ {l => Block(l)}
	def statement : Parser[Statement] = whileLoop | ifelse | assignment | skip | block

	def whileLoop = ("while(" ~> bExp <~ ")" ) ~ statement ^^
		{ case conditional ~ body => While(conditional, body) }

	def assignment = (identifier <~ ":=") ~ aExp ^^ {
		case id ~ e => Assig(id, e)
	}

	def ifelse = ("if" ~> bExp) ~ ("then" ~> statement) ~ ("else" ~> statement) ^^ {
		case bexp ~ s1 ~ s2 => IfElse(bexp, s1, s2)
	}

	def skip = "skip" ^^ {case _ => Skip}
}

trait BExp
case object True extends BExp
case object False extends BExp
case class Not(e1: BExp) extends BExp
case class BinBExp(operator: String, e1: BExp, e2: BExp) extends BExp
case class RelationalExp(operator: String, e1: AExp, e2: AExp) extends BExp

trait AExp
case class BinOp(operator: String, e1: AExp, e2: AExp) extends AExp
case class INT(value : Int) extends AExp
case class Ref(id: String) extends AExp

trait Statement
case class Block(statements: List[Statement]) extends Statement
case class While(conditional: BExp, stmt: Statement) extends Statement
case class Assig(ref: String, exp: AExp) extends Statement
case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement
case object Skip extends Statement