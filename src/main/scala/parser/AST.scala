package parser

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