package models

import java.time.ZonedDateTime
import java.util.UUID
import javax.inject._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import play.api.libs.json._
import play.api.Logger

import models.check_request._
import models.token_module.TokenDAO

class AuthorizationChecker @Inject()(
    tokenDAO: TokenDAO
) {
    def generateHMAC(message: String, key: String): String = {
        val secret = new SecretKeySpec(key.getBytes, "HmacSHA256")
        val mac = Mac.getInstance("HmacSHA256")
        mac.init(secret)
        val hashString: Array[Byte] = mac.doFinal(message.getBytes)
        new String(hashString.map(_.toChar))
    }

    def check(request: CheckRequest): Either[_, Unit] = {
        tokenDAO.getTokenByUserUUID(request.userId) match {
            case Some(token) => {
                val message = request.userId + token.accessToken.toString + request.date
                val hmac = generateHMAC(message, token.accessToken.toString)
                Logger.debug(request.signature)
                Logger.debug(hmac)
                if (request.signature == hmac) Right(())
                else Left(None)
            }
            case None => Left(None)
        }
    }
}