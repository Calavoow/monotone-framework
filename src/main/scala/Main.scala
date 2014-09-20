import parser.WhileParser

object Main {
	def main(args: Array[String]) {
		val parsed = WhileParser.parseAll(WhileParser.statement, "{x:=1 while(1 == 1){x := 1 x:=2}}")
		parsed match {
			case WhileParser.Success(prog, _) => println(prog)
			case x => println(x)
		}
	}
}