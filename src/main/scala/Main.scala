import parser.{AstUtils, WhileParser}

object Main {
	def main(args: Array[String]) {
//		val toParse = "begin if true && true || false then x:=1 else { x:= 1 y:=2 } end"
		val toParse = "begin proc p(val x; res y) is y:=x end call p(x;y) end"
//		val toParse = "begin {x:=1 y:=2} end"
		val parsed = WhileParser.parseAll(WhileParser.program, toParse)
		parsed match {
			case WhileParser.Success(prog, _) => println(prog)
			case x => println(x)
		}

		val prog = parsed.get
		AstUtils.labelNodes(prog)
		val procs = AstUtils.mapProcedures(prog)
		println(procs)
		println(prog.pp)
		println(prog.flow)
		println(prog.reverseFlow)
		println(prog.interFlow)

		println(AstUtils.labelToNode(prog))
	}
}