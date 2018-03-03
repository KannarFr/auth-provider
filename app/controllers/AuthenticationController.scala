package controllers

import java.util.UUID
import javax.inject._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.json._

import models.AuthorizationChecker
import models.check_request._
import models.token_module._
import models.user_module._

@Singleton
class AuthenticationController @Inject()(
    authChecker: AuthorizationChecker,
    cc: ControllerComponents,
    implicit val ec: ExecutionContext,
    tokenDAO: TokenDAO,
    userDAO: UserDAO,
    userManager: UserManager,
    tokenManager: TokenManager
) extends AbstractController(cc) {
    def grant = Action.async { implicit request =>
        val encoded = (for {
            authorization <- request.headers.get("Authorization")
            encoded <- authorization.split(" ").drop(1).headOption
        } yield { encoded })
        
        encoded match {
            case Some(encoded: String) => {
                userManager.authenticate(encoded) match {
                    case Right(token) => Future(Ok(Json.toJson(token)))
                    case _ => Future(Unauthorized)
                }
            }
            case _ => Future(Unauthorized)
        }
    }

    def check = Action.async(parse.json[CheckRequest]) { implicit request =>
        val requestToCheck = request.body
        authChecker.check(requestToCheck) match {
            case Left(_) => Future(Unauthorized)
            case Right(_) => Future(Ok)
        }
    }

    def renew = Action.async(parse.json[Token]) { implicit request =>
        val token = request.body
        tokenManager.renew(token) match {
            case Left(_) => Future(BadRequest)
            case Right(_) => Future(Ok(Json.toJson(token)))
        }
    }
}