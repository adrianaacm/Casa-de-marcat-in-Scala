package database 

import scala.language.implicitConversions

// 2.2.1
trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    r.get(colName) match {
      case None => None
      case Some(value) => Some(predicate(value))
    }
  }
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    def compoundLoop(conditionsLeft: List[FilterCond], currentState: Option[Boolean]) : Option[Boolean] = {
      conditionsLeft match {
        case Nil => currentState
        case x::xs => {
          x.eval(r) match {
            case None => None
            case Some(value) => compoundLoop(xs, Some(op(value, currentState.get)))
          }
        }
      }
    }
    compoundLoop(conditions, Option(false))
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    f.eval(r) match {
      case None => None
      case Some(true) => Some(false)
      case Some(false) => Some(true)
    }
  }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ && _, List(f1, f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Compound( _ || _, List(f1,f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound(_ == _, List(f1,f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = Compound(_ || _, fs).eval(r)
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = Compound(_ && _, fs).eval(r)
}

// 2.2.2
extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def unary_! = Not(f)
}

// 2.2.3
implicit def tuple2Field(t: (String, String => Boolean)): Field = Field(t._1, t._2)

extension(t: Table) {
    // 2.2.4
    def filter(f: FilterCond): Table = {
      def filterLoop(rows: List[Row], curr: Table): Table = {
        rows match {
          case Nil => curr
          case r::rs => {
            if (f.eval(r) == None || f.eval(r) == Some(false)) filterLoop(rs, curr.delete(r))
            else filterLoop(rs, curr)
          }
        }
      }
      filterLoop(t.data, t)
    }

    // 2.2.5
    def sortNums(column: String, ascending: Boolean = true): Table = {
      t.tableData.sortBy(row => row.getOrElse(column, "0").toInt) match {
        case sorted if ascending => Table(t.tableName, sorted)
        case sorted => Table(t.tableName, sorted.reverse)
      }
    }

    def update(f: FilterCond, updates: Map[String, String]): Table = {
      def updateLoop(rows: List[Row], curr: Table): Table = {
        rows match {
          case Nil => curr
          case r::rs => {
            if (f.eval(r) == Some(true)) {
              val updatedRow = r ++ updates
              updateLoop(rs, curr.delete(r).insert(updatedRow))
            }
            else updateLoop(rs, curr)
          }
        }
      }
      updateLoop(t.data, t).sortNums("id")
    }
}