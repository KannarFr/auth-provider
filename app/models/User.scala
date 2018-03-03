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
import org.apache.commons.codec.binary.Base64
import java.time.LocalDateTime

import models.token_module._

object user_module {
    case class User(
        uuid: UUID,
        email: String,
        password: String,
        creation_date: LocalDateTime
    )

    sealed abstract class UserError
    case object UserAlreadyPresent extends UserError
    case object UnhandledException extends UserError
    case object UserNotFound extends UserError
    case object AuthorizationNotValid extends UserError
    case object WrongPassword extends UserError
    case object TokenCreationForUserDoesntWork extends UserError

    object User {
        implicit val userFormat = Json.format[User]
        implicit val userPgEntity: PgEntity[User] = new PgEntity[User] {
            val tableName = "users"

            val columns = List(
                PgField("uuid"), PgField("email"), PgField("password"),
                PgField("creation_date")
            )

            def parser(prefix: String): RowParser[User] = {
                get[UUID]("uuid") ~
                get[String]("email") ~
                get[String]("password") ~
                get[LocalDateTime]("creation_date") map {
                    case (uuid ~ email ~ password ~ creation_date) =>
                        User(uuid, email, password, creation_date)
                }
            }
        }
    }

    class UserManager @Inject()(
        userDAO: UserDAO,
        tokenDAO: TokenDAO,
        tokenManager: TokenManager
    ) {
        def authenticate(encoded: String): Either[UserError, Token] = {
            new String(Base64.decodeBase64(encoded.getBytes)).split(":").toList match {
                case email :: pwd :: Nil => {
                    userDAO.getUserByEmail(email) match {
                        case Some(user) => {
                            if (user.password == pwd) {
                                val token = tokenManager.generateTo(user)
                                tokenDAO.create(token) match {
                                    case Right(_) => Right(token)
                                    case _ => Left(TokenCreationForUserDoesntWork)
                                }
                            } else {
                                Left(WrongPassword)
                            }
                        }
                        case _ => Left(UserNotFound)
                    }               
                }
                case _ => Left(AuthorizationNotValid)
            }
        }
    }

    class UserDAO @Inject()(db: Database) {
        import User._
        
        def getUserById(uuid: UUID): Option[User] = db.withConnection { implicit c =>
            SQL(selectSQL[User] + " WHERE uuid = {uuid}").on(
                'uuid -> uuid
            ).as(parser[User]().singleOpt)
        }

        def getUserByEmail(email: String): Option[User] = db.withConnection { implicit c =>
            SQL(selectSQL[User] + " WHERE email = {email}").on(
                'email -> email
            ).as(parser[User]().singleOpt)
        }

        def patchUser(user: User): Either[UserError, Unit] = db.withConnection { implicit c =>
            Try {
                SQL(updateSQL[User](List("uuid", "creation_date"))).on(
                    'uuid -> user.uuid,
                    'email -> user.email,
                    'password -> user.password
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

        def create(user: User): Either[UserError, Unit] = db.withConnection { implicit c => 
            Try {
                SQL(insertSQL[User]).on(
                    'uuid -> user.uuid,
                    'email -> user.email,
                    'password -> user.password,
                    'creation_date -> user.creation_date
                ).executeUpdate
                ()
            } match {
                case Failure(e: org.postgresql.util.PSQLException) => {
                    if(e.getSQLState == "23505") {
                        Left(UserAlreadyPresent)
                    } else {
                        Logger.error("Something wrong happened while inserting a user")
                        e.printStackTrace
                        Left(UnhandledException)
                    }
                }
                case Failure(e) => {
                    Logger.error("Something wrong happened while inserting a user")
                    e.printStackTrace
                    Left(UnhandledException)
                }
                case Success(s) => Right(s)
            }
        }
    }
}