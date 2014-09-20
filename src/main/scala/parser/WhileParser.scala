package parser

import scala.util.parsing.combinator.RegexParsers

object WhileParser extends RegexParsers {

	override type Elem = Char

	def identifier = """[a-zA-Z][a-zA-Z0-9]*""".r
	def integer = """(0|[1-9]\d*)""".r ^^ {_.toInt}

	def op_a = """[\-\+\*]""".r ^^ {_.toString}
	def op_r = """<|>|==""".r ^^ {_.toString}
	def op_b = """&&|\|\|""".r ^^ {_.toString}

//	def exp : Parser[Expression] = (integer ^^ {INT(_)}) | (exp ~ op ~ exp ^^ {
//		case e1 ~ operator ~ e2 => BinOp(operator, e1, e2)
//	})

	def aExp: Parser[AExp] = term ~ (
		op_a ~ aExp
	  ).? ^^ {
		case f1 ~ Some(op ~ f2) => BinOp(op, f1, f2)
		case f1 ~ None => f1
	}
	def term : Parser[AExp] = (
		("(" ~> aExp <~ ")")
		| (identifier ^^ Ref)
		| (integer ^^ INT)
	)

	def bExp: Parser[BExp] = (
		binValue ~ (op_b ~ bExp).? ^^ {
			case b1 ~ Some(op ~ b2) => BinBExp(op, b1, b2)
			case b1 ~ None => b1
		}
		| "not" ~> bExp ^^ {case e => Not(e)}
		| aExp ~ op_r ~ aExp ^^ { case e1 ~ op ~ e2 => RelationalExp(op, e1, e2)}
	)

	def binValue = "true" ^^ {case _ => True} | "false" ^^ {case _ => False}


	def statements = statement*
	def block = "{" ~> statements <~ "}" ^^ {l => Block(l)}
	def statement : Parser[Statement] = whileLoop | block | assignment

	def assignment = (identifier <~ ":=") ~ aExp ^^ {
		case id ~ e => Assig(id, e)
	}
	def whileLoop = ("while(" ~> bExp <~ ")" ) ~ statement ^^
	  { case conditional ~ body => While(conditional, body) }

}

abstract trait BExp
case object True extends BExp
case object False extends BExp
case class Not(e1: BExp) extends BExp
case class BinBExp(operator: String, e1: BExp, e2: BExp) extends BExp
case class RelationalExp(operator: String, e1: AExp, e2: AExp) extends BExp

abstract trait AExp
case class BinOp(operator: String, e1: AExp, e2: AExp) extends AExp
case class INT(value : Int) extends AExp
case class Ref(id: String) extends AExp

abstract trait Statement
case class Block(statements: List[Statement]) extends Statement
case class While(conditional: BExp, stmt: Statement) extends Statement
case class Assig(ref: String, exp: AExp) extends Statement
case class IfElse(condition: BExp, s1: Statement, s2: Statement) extends Statement