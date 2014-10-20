import parser.WhileParser

import shapeless._
import poly._

object Main {
	def main(args: Array[String]) {
    println(util.Properties.versionString)
		val parsed = WhileParser.parseAll(WhileParser.statement, "if true && true || false then x:=1 else { x:= 1 * 2 + 3 }")
		parsed match {
			case WhileParser.Success(prog, _) => println(prog)
			case x => println(x)
		}

		val prog = parsed.get
		Flow.labelNodes(prog)
		println(prog)
	}
}