import analyzer.Monotone
import parser.WhileParser

object Main {
	def main(args: Array[String]) {
        println(util.Properties.versionString)
		val toParse = "if true && true || false then x:=1 else { x:= 1 y:=2 }"
//		val toParse = "{x:=1 y:=2}"
		val parsed = WhileParser.parseAll(WhileParser.statement, toParse)
		parsed match {
			case WhileParser.Success(prog, _) => println(prog)
			case x => println(x)
		}

		val prog = parsed.get
		Monotone.labelNodes(prog)
		println(prog.pp)
		println(prog.flow)
		println(prog.reverseFlow)

		println(Monotone.labelToNode(prog))
	}
}