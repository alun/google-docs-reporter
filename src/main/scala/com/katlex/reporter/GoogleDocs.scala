package com.katlex.reporter

import com.google.api.client.googleapis.GoogleTransport
import com.google.api.client.googleapis.json.JsonCParser
import com.google.api.client.http.HttpResponseException
import com.google.api.client.googleapis.auth.clientlogin.ClientLogin
import org.apache.http.conn.ConnectTimeoutException
import java.net.UnknownHostException

object GoogleDocs extends Application {
  private def obtainUserAndPassword = {
    val Array(user, passwd) = Array("google.user", "google.password").
                                    map(System.getProperty)
    if (user == null || passwd == null) {
      println("""Please, specify user name and password
        |for google connetion through java system properties
        |-Dgoogle.user and -Dgoogle.password""".stripMargin)
      System.exit(1)
    }
    (user, passwd)
  }

  val transport = GoogleTransport.create
  transport.addParser(new JsonCParser)
  println()
  val result = try {
    val (user, passwd) = obtainUserAndPassword

    val a = new ClientLogin
    a.authTokenType = "wise"
    a.username = user
    a.password = passwd
    a.applicationName = "katlex-wise_client-1"
    a.accountType = "HOSTED_OR_GOOGLE"
    a.authenticate.setAuthorizationHeader(transport)

    val rq = transport.buildGetRequest
    rq.setUrl("https://spreadsheets.google.com/feeds/spreadsheets/private/full")
    Left(rq.execute.parseAsString)
  } catch {
    case e: HttpResponseException =>
      val info = e.response.parseAs(classOf[ClientLogin.ErrorInfo])
      Right(info.error)
    case _: ConnectTimeoutException =>
      Right("Host connetion is timed out")
    case e: UnknownHostException =>
      Right("Unknown host: " + e.getMessage)
  }

  result match {
    case Left(msg) => println(msg)
    case Right(msg) =>
      Console.err.println(msg)
      System.exit(2)
  }


}