import parser.WhileParser

object Main {
	def main(args: Array[String]) {
		val parsed = WhileParser.parseAll(WhileParser.statement, "if true && true || false then x:=1 else x:=2")
		parsed match {
			case WhileParser.Success(prog, _) => println(prog)
			case x => println(x)
		}
	}
}