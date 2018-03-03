package models

import java.time.ZonedDateTime
import java.util.UUID

import play.api.libs.json._

object check_request {
  case class CheckRequest(
    method: String,
    url: String,
    userId: UUID,
    date: ZonedDateTime,
    signature: String
  )

  implicit val checkRequestWrites = Json.writes[CheckRequest]
  implicit val checkRequestReads = Json.reads[CheckRequest]
}