package database 

import scala.annotation.tailrec

type Row = Map[String, String]
type Tabular = List[Row]

object Table {
  // 2.1.1
  def apply(name: String, s: String): Table = {
    val all = s.split("\n").toList;
    val header = all.head.split(",").toList;
    
    val tableData = all.tail.map(row => header.zip(row.split(",").toList).toMap) 
    new Table(name, tableData)
  }
}

case class Table (tableName: String, tableData: Tabular) {

  def header: List[String] = tableData.head.map(x => x._1).toList
  def data: Tabular = tableData
  def name: String = tableName

  // 2.1.2
  override def toString: String = {
    val headerLine = header.mkString(",")
  
    val dataLines = tableData.map { row =>
      header.map(col => row.getOrElse(col, "")).mkString(",")
    }
  
    (headerLine :: dataLines).mkString("\n")
  }

  // 2.1.3
  def insert(row: Row): Table = {
    if (tableData.contains(row)) Table(tableName, tableData)
    else Table(tableName, tableData :+ row)
  }

  // 2.1.4
  def delete(row: Row): Table = {
    def deleteLoop(newTableData: Tabular, rest: Tabular): Tabular = {
      rest match {
        case Nil => newTableData
        case r::rs => {
          if (r == row) deleteLoop(newTableData, rs)
          else deleteLoop(newTableData :+ r, rs)
        }
      }
    }

    Table(tableName, deleteLoop(Nil, tableData))
  }

  // 2.1.5
  def sort(column: String, ascending: Boolean = true): Table = {
    tableData.sortBy(row => row.getOrElse(column, "")) match {
      case sorted if ascending => Table(tableName, sorted)
      case sorted => Table(tableName, sorted.reverse)
    }
  }

  // 2.1.6
  def select(columns: List[String]): Table = {
    val newTableData = tableData.map(row => row.filterKeys(c => columns.contains(c)).toMap)
    Table(tableName, newTableData)
  }
}

extension (table: Table) {
  def apply(i: Int): Row = {
    table.data(i)
  }
}
