package com.katlex.reporter

import com.google.api.client.googleapis.GoogleTransport
import com.google.api.client.googleapis.json.JsonCParser
import com.google.api.client.http.HttpResponseException
import com.google.api.client.googleapis.auth.clientlogin.ClientLogin
import org.apache.http.conn.ConnectTimeoutException
import java.net.UnknownHostException

object GoogleDocs extends Application {
  val transport = GoogleTransport.create
  transport.addParser(new JsonCParser)
  try {
    val a = new ClientLogin
    a.authTokenType = "wise"
    a.username = "alexey.lunacharsky@stdva.com"
    a.password = "x"
    a.authenticate.setAuthorizationHeader(transport)

    val rq = transport.buildGetRequest
    rq.setUrl("https://spreadsheets.google.com/feeds/spreadsheets/private/full")
    println(rq.execute.parseAsString)
  } catch {
    case e: HttpResponseException =>
      Console.err.println(e.response.parseAsString)
      throw e
    case _: ConnectTimeoutException =>
      Console.err.println("Host connetion is timedout")
      System.exit(1)
    case e: UnknownHostException =>
      Console.err.println("Unknown host: " + e.getMessage)
      System.exit(1)
  }
}