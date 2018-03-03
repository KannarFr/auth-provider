package models.pg

import anorm.RowParser

object pg_entity_module {
  case class PgField(name: String, annotation: Option[String] = None)

  trait PgEntity[A] {
    def tableName: String
    def columns: List[PgField]
    def columnList(prefix: Option[String]) = {
      val p = prefix.getOrElse(tableName + ".")
      columns.map(p + _.name).mkString(", ")
    }
    def parser(prefix: String): RowParser[A]
  }

  def columns[A](implicit ev: PgEntity[A]) = ev.columns
  def columnList[A](prefix: Option[String] = None)(implicit ev: PgEntity[A]) =
    ev.columnList(prefix)
  def tableName[A](implicit ev: PgEntity[A]) = ev.tableName

  def selectSQL[A](implicit ev: PgEntity[A]) = {
    val tablename = ev.tableName
    val columns = ev.columns.map(_.name).mkString(",")
    s"select $columns from $tablename"
  }

  def prefixedSelectSQL[A](implicit ev: PgEntity[A]) = {
    val tablename = ev.tableName
    val columns = ev.columns.map(c => tablename + "." + c.name).mkString(",")
    s"select $columns from $tablename"
  }

  def insertSQL[A](implicit ev: PgEntity[A]) = {
    val tablename = ev.tableName
    val columns = ev.columns.map(_.name).mkString("(", ",", ")")
    val values = ev.columns
      .map(c =>
        "{" + c.name + "}" + (c.annotation map (a => "::" + a) getOrElse ""))
      .mkString("(", ",", ")")
    s"insert into $tableName $columns values $values"
  }

  def updateSQL[A](ignored: List[String] = List())(implicit ev: PgEntity[A]) = {
    val tablename = ev.tableName
    val columns = ev.columns.tail.filterNot(c => ignored.contains(c.name))
    val updates =
      columns
        .map(c =>
          c.name + " = " + "{" + c.name + "}" + (c.annotation map (a =>
            "::" + a) getOrElse ""))
        .mkString(", ")
    val pk = ev.columns.headOption.map(_.name).getOrElse("")
    s"UPDATE $tablename SET $updates WHERE $pk = {$pk}"
  }

  def deleteSQL[A](implicit ev: PgEntity[A]) = {
    val tablename = ev.tableName
    val pk = ev.columns.headOption.map(_.name).getOrElse("")
    s"DELETE FROM $tablename WHERE $pk = {$pk}"
  }

  def parser[A](prefix: Option[String] = None)(implicit ev: PgEntity[A]) = {
    ev.parser(prefix getOrElse (ev.tableName + "."))
  }
}
