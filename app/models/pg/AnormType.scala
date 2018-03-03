package models.pg

import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import org.postgresql.util.PGobject
import org.postgresql.geometric.PGpoint
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import java.util.UUID
import org.joda.time.Instant
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.LocalDate
import java.sql.Timestamp
import scala.Left
import scala.Right

object AnormType {

  implicit def rowToPGPoint: Column[PGpoint] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case p: PGpoint => Right(p)
      case _ =>
        Left(
          TypeDoesNotMatch(
            "Cannot convert " + value + ":" + value
              .asInstanceOf[AnyRef]
              .getClass + " to PGPoint for column " + qualified))
    }
  }

  implicit val pgPointReads = new Reads[PGpoint] {
    def reads(js: JsValue): JsResult[PGpoint] = {
      val array = js.as[String].tail.init.split(",")
      (for {
        x <- scala.util.Try(array.head.toDouble).toOption
        y <- scala.util.Try(array.last.toDouble).toOption
      } yield {
        JsSuccess(new PGpoint(x, y))
      }) getOrElse JsError(Seq())
    }
  }

  implicit def rowToJsValue: Column[JsValue] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case o: PGobject if o.getType() == "json" =>
        Right(Json.parse(o.getValue()))
      case _ =>
        Left(
          TypeDoesNotMatch(
            "Cannot convert " + value + ":" + value
              .asInstanceOf[AnyRef]
              .getClass + " to json for column " + qualified))
    }
  }

  // Column read
  implicit def rowToUUID: Column[UUID] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case d: UUID => Right(d)
      case _ =>
        Left(
          TypeDoesNotMatch(
            "Cannot convert " + value + ":" + value
              .asInstanceOf[AnyRef]
              .getClass + " to UUID for column " + qualified))
    }
  }

  implicit def rowToInstant: Column[Instant] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case d: Instant   => Right(d)
      case d: Timestamp => Right(new Instant(d))
      case _ =>
        Left(
          TypeDoesNotMatch(
            "Cannot convert " + value + ":" + value
              .asInstanceOf[AnyRef]
              .getClass + " to Instant for column " + qualified))
    }
  }

  // statement write
  implicit val uuidToStatement = new ToStatement[UUID] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: UUID): Unit =
      s.setObject(index, aValue)
  }

  implicit val pgpointToStatement = new ToStatement[PGpoint] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: PGpoint): Unit =
      s.setObject(index, aValue)
  }

  implicit val jsonToStatement = new ToStatement[JsValue] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: JsValue): Unit = {
      val pgo = new PGobject()
      pgo.setType("json")
      pgo.setValue(aValue.toString)

      s.setObject(index, pgo)
    }
  }

  // statement write
  implicit val instantToStatement = new ToStatement[Instant] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: Instant): Unit =
      s.setTimestamp(index, new java.sql.Timestamp(aValue.toDate.getTime))
//    def set(s: java.sql.PreparedStatement, index: Int, aValue: Instant): Unit = s.setObject(index, "TIMESTAMP '" + dateToSQL(aValue) + "'")
  }

  // utils
  private val fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss'")
  private val fmtDate = DateTimeFormat.forPattern("yyyy-MM-dd")

  def dateToSQL(i: Instant): String = {
    fmt print i
  }

  implicit def rowToDateTime: Column[DateTime] = Column.nonNull {
    (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case ts: java.sql.Timestamp => Right(new DateTime(ts.getTime))
        case d: java.sql.Date       => Right(new DateTime(d.getTime))
        case str: String            => Right(fmt.parseDateTime(str))
        case _ =>
          Left(
            TypeDoesNotMatch(
              "Cannot convert " + value + ":" + value
                .asInstanceOf[AnyRef]
                .getClass))
      }
  }

  implicit val dateTimeToStatement = new ToStatement[DateTime] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: DateTime): Unit = {
      s.setTimestamp(
        index,
        new java.sql.Timestamp(aValue.withMillisOfSecond(0).getMillis()))
    }
  }

  implicit def rowToLocalDate: Column[LocalDate] = Column.nonNull {
    (value, meta) =>
      val MetaDataItem(qualified, nullable, clazz) = meta
      value match {
        case ts: java.sql.Timestamp => Right(new LocalDate(ts.getTime))
        case d: java.sql.Date       => Right(new LocalDate(d.getTime))
        case str: String            => Right(fmtDate.parseLocalDate(str))
        case _ =>
          Left(
            TypeDoesNotMatch(
              "Cannot convert " + value + ":" + value
                .asInstanceOf[AnyRef]
                .getClass))
      }
  }

  implicit val localDateToStatement = new ToStatement[LocalDate] {
    def set(s: java.sql.PreparedStatement,
            index: Int,
            aValue: LocalDate): Unit = {
      s.setDate(index, new java.sql.Date(aValue.toDate.getTime))
    }
  }

  implicit def rowToList[A]: Column[List[A]] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case arr: java.sql.Array =>
        Right(arr.getArray.asInstanceOf[Array[A]].toList)
      case o: org.postgresql.util.PGobject => Right(List[A]())
      case _ =>
        Left(
          TypeDoesNotMatch(
            "Cannot convert " + value + ":" + value
              .asInstanceOf[AnyRef]
              .getClass))
    }
  }

  implicit def listToStatement[A] = new ToStatement[List[A]] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: List[A]): Unit = {
      s.setString(index, aValue.mkString("{", ", ", "}"))
    }
  }
}
