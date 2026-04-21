package database 

import scala.language.implicitConversions

// 2.3.1
trait PP_SQL_Table{
  def eval: Option[Table]
}

case class InsertRow(table:Table, values: Tabular) extends PP_SQL_Table{
  def eval: Option[Table] = {
    def whileValues(valuesLeft: Tabular, newTable: Table): Table = {
      valuesLeft match {
        case Nil => newTable
        case v::vs => whileValues(vs, newTable.insert(v))
      }
    }
    Some(whileValues(values, table))
  }
}

case class SortTable(table: Table, column: String) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.sort(column))
}

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.update(condition, updates))

}

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.delete(row))

}

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.filter(condition))

}

case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table{
  def eval: Option[Table] = Some(table.select(columns))

}


// 2.3.2
implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] = Some(InsertRow(t._1.get, t._3))
 
implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] = Some(SortTable(t._1.get, t._3))
 
implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] = Some(UpdateRow(t._1.get, t._3, t._4))
 
implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] = Some(DeleteRow(t._1.get, t._3))
 
implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] = Some(FilterRows(t._1.get, t._3))
 
implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] = Some(SelectColumns(t._1.get, t._3))


def queryT(p: Option[PP_SQL_Table]): Option[Table] = p match {
  case Some(pp) => pp.eval
  case _ => None
}