package parser

import parser.AST._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

object WhileParser extends RegexParsers with PackratParsers {

	override type Elem = Char

	lazy val identifier: PackratParser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {_.toString}
	lazy val identifierList: PackratParser[List[String]] = identifier ~ ("," ~> identifier).* ^^ {
		case id ~ ids => id :: ids
	}
	lazy val integer: PackratParser[Int] = """(0|[1-9]\d*)""".r ^^ {_.toInt}

	lazy val op_a: PackratParser[String] = """[\-\+\*]""".r ^^ {_.toString}
	lazy val op_r: PackratParser[String] = """<|>|==""".r ^^ {_.toString}
	lazy val op_b: PackratParser[String] = """&&|\|\|""".r ^^ {_.toString}
	def precedence(op: String) : Int = op match {
		case "*" => 6
		case "+" | "-" => 5
	}

	lazy val aExp: PackratParser[AExp] = nestedAExp | aExp ~ op_a ~ aTerm ^^ {
		case e1 ~ op ~ e2 => BinOp(op, e1, e2)
	} | aTerm

	lazy val nestedAExp: PackratParser[AExp] = aExp ~ op_a ~ aTerm ~ op_a ~ aTerm ^^ {
		case e1 ~ op1 ~ e2 ~ op2 ~ e3 if precedence(op2) > precedence(op1) => BinOp(op1, e1, BinOp(op2, e2, e3))
		case e1 ~ op1 ~ e2 ~ op2 ~ e3 => BinOp(op2, BinOp(op1, e1, e2), e3)
	}

	lazy val aTerm : PackratParser[AExp] = (
		("(" ~> aExp <~ ")")
			| (identifier ^^ { case id => Ref(id) })
			| (integer ^^ { case int => INT(int)})
	)

	lazy val bExp: PackratParser[BExp] = (
		bExp ~ op_b ~ binTerm ^^ {
			case e1 ~ op ~ e2 => BinBExp(op, e1, e2)
		}
			| "not " ~> bExp ^^ {case e => Not(e)}
			| binTerm
	)

	lazy val binTerm : PackratParser[BExp] = (
		"true" ^^ {case _ => True()}
			| "false" ^^ {case _ => False()}
			| aExp ~ op_r ~ aExp ^^ { case e1 ~ op ~ e2 => RelationalExp(op, e1, e2)}
	)

	/**
	 * Statements must contain at least 2 statements, so that it is a sequence.
	 */
	lazy val statements : PackratParser[List[Statement]] = statement ~ statement.+ ^^ {case s1 ~ s2s => s1 +: s2s}
	lazy val block : PackratParser[Statement] = "{" ~> statements <~ "}" ^^ {case l => Sequence(l)}
	lazy val statement : PackratParser[Statement] = whileLoop | ifelse | assignment | skip | block | call

	lazy val whileLoop : PackratParser[Statement] = ("while(" ~> bExp <~ ")" ) ~ statement ^^
		{ case conditional ~ body => While(conditional, body) }

	lazy val assignment : PackratParser[Statement] = (identifier <~ ":=") ~ aExp ^^ {
		case id ~ e => Assig(id, e)
	}

	lazy val ifelse : PackratParser[Statement] = ("if " ~> bExp) ~ ("then " ~> statement) ~ ("else " ~> statement) ^^ {
		case bexp ~ s1 ~ s2 => IfElse(bexp, s1, s2)
	}

	lazy val skip = "skip" ^^ {case _ => Skip()}

	lazy val call: PackratParser[Call] = ("call " ~> identifier <~ "(") ~ identifierList ~ (";" ~> identifier <~ ")") ^^ {
		case procName ~ arguments ~ result => Call(procName, arguments, result)
	}

	lazy val program : PackratParser[Program] = ("begin " ~> procedure.*) ~ (statement <~ "end") ^^ {
		case procs ~ stmt => Program(procs, stmt)
	}
	lazy val procedure: PackratParser[Procedure] =
		("proc " ~> identifier <~ "(val ") ~ identifierList ~ (";" ~> "res " ~> identifier) ~ (") is " ~> statement <~ "end") ^^ {
			case procName ~ variables ~ result ~ stmnt => Procedure(procName, variables, result, stmnt)
		}
}