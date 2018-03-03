package models

import javax.inject._
import play.api.db._
import anorm._
import anorm.SqlParser._
import models.pg.pg_entity_module._
import models.pg.AnormType._
import play.api.libs.json._
import java.util.UUID
import scala.util.{Try, Success, Failure}
import play.api.Logger
import java.time.ZonedDateTime

import models.user_module._

object token_module {
    case class Token(
        accessToken: UUID,
        refreshToken: UUID,
        creation: ZonedDateTime,
        expire: ZonedDateTime,
        user: User
    )

    sealed abstract class TokenError
    case object TokenAlreadyPresent extends TokenError
    case object UnhandledException extends TokenError
    case object TokenDoesntExist extends TokenError
    case object TokenExpired extends TokenError

    object Token {
        implicit val tokenFormat = Json.format[Token]
        implicit val tokenPgEntity: PgEntity[Token] = new PgEntity[Token] {
            val tableName = "tokens"

            val columns = List(
                PgField("access_token"), PgField("refresh_token"),
                PgField("creation_date"), PgField("end_date"), PgField("user_uuid") 
            )

            def parser(prefix: String): RowParser[Token] = {
                get[UUID]("access_token") ~
                get[UUID]("refresh_token") ~
                get[ZonedDateTime]("creation_date") ~
                get[ZonedDateTime]("end_date") ~
                get[JsValue]("user_js") map {
                    case (
                        access_token ~ refresh_token ~ creation_date ~ end_date ~ user_js
                    ) => Token(
                        access_token, refresh_token, creation_date, end_date, user_js.as[User]
                    )
                }
            }
        }
    }

    class TokenManager @Inject()(tokenDAO: TokenDAO) {
        def generateTo(user: User) = {
            Token(
                accessToken = UUID.randomUUID,
                refreshToken = UUID.randomUUID,
                creation = ZonedDateTime.now,
                expire = ZonedDateTime.now.plusSeconds(3600),
                user = user
            )
        }

        def check(token: Token): Either[TokenError, Unit] = {
            tokenDAO.getTokenBy(token.accessToken) match {
                case None => Left(TokenDoesntExist)
                case Some(token: Token) => {
                    if (ZonedDateTime.now isAfter token.expire) {
                        Left(TokenExpired)
                    } else {
                        Right(())
                    }
                }
            }
        }

        def renew(token: Token): Either[TokenError, Token] = {
            tokenDAO.patchToken(token) match {
                case Left(e) => Left(e)
                case Right(_) => {
                    tokenDAO.getTokenBy(token.accessToken) match {
                        case Some(token: Token) => Right(token)
                        case _ => Left(TokenDoesntExist)
                    }
                }
            }
        }
    }

    class TokenDAO @Inject()(db: Database) {
        import Token._

        def getTokenByUserUUID(uuid: UUID): Option[Token] = db.withConnection { implicit c =>
            SQL(s"""
                SELECT t.*, row_to_json(u.*) as user_js
                FROM tokens t
                WHERE t.user_uuid = {uuid}
            """).on(
                'uuid -> uuid.toString
            ).as(parser[Token]().singleOpt)
        }

        def getTokenBy(accessToken: UUID): Option[Token] = db.withConnection { implicit c =>
            SQL(s"""
                SELECT t.*, row_to_json(u.*) as user_js
                FROM users u, tokens t
                WHERE t.user_uuid = u.uuid
                AND t.access_token = {access_token}
            """).on(
                'access_token -> accessToken.toString
            ).as(parser[Token]().singleOpt)
        }

        def patchToken(token: Token): Either[TokenError, Unit] = db.withConnection { implicit c =>
            Try {
                SQL(updateSQL[Token](List("access_token", "creation_date"))).on(
                    'access_token -> token.accessToken,
                    'end_date -> ZonedDateTime.now.plusSeconds(3600),
                    'refresh_token -> UUID.randomUUID
                ).executeUpdate
                ()
            } match {
                case Failure(e) => {
                    Logger.error("Something wrong happened while patching a user")
                    e.printStackTrace
                    Left(UnhandledException)
                }
                case Success(s) => Right(s)
            }
        }

        def create(token: Token): Either[TokenError, Unit] = db.withConnection { implicit c => 
            Try {
                SQL(insertSQL[Token]).on(
                    'access_token -> token.accessToken,
                    'refresh_token -> token.refreshToken,
                    'creation_date -> token.creation,
                    'end_date -> token.expire,
                    'user_uuid -> token.user.uuid
                ).executeUpdate
                ()
            } match {
                case Failure(e: org.postgresql.util.PSQLException) => {
                    if(e.getSQLState == "23505") {
                        Left(TokenAlreadyPresent)
                    } else {
                        Logger.error("Something wrong happened while inserting a token")
                        e.printStackTrace
                        Left(UnhandledException)
                    }
                }
                case Failure(e) => {
                    Logger.error("Something wrong happened while inserting a token")
                    e.printStackTrace
                    Left(UnhandledException)
                }
                case Success(s) => Right(s)
            }
        }
    }
}