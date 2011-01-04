package com.katlex.reporter

import com.google.api.client.googleapis.GoogleTransport
import com.google.api.client.googleapis.json.JsonCParser
import com.google.api.client.googleapis.auth.clientlogin.ClientLogin
import org.apache.http.conn.ConnectTimeoutException
import java.net.UnknownHostException
import java.text.SimpleDateFormat
import java.util.{Locale, TimeZone, Calendar, Date}
import xml.{Node, XML}
import collection.immutable.Seq
import com.google.api.client.http.{HttpContent, HttpResponseException}
import java.io.OutputStream

object StringSubstitution {
  implicit def stringSubstitute(s: String) = new Object {
    def |(substitutions: { def toString: String }*) = {
      val replacements = for {
        (s, i) <- substitutions.zipWithIndex
        ph = "\\{" + i + "\\}"
      } yield (ph, s.toString)

      replacements.foldLeft(s) {
        (source, repl) => val (placeHolder, substitution) = repl
        source.replaceAll(placeHolder, substitution)
      }
    }
  }
}
import StringSubstitution._

object GoogleClient {
  val transport = GoogleTransport.create
  transport.addParser(new JsonCParser)

  def login(username: String, password: String) {
    val a = new ClientLogin
    a.authTokenType = "wise"
    a.username = username
    a.password = password
    a.applicationName = "katlex-wise_client-1"
    a.accountType = "HOSTED_OR_GOOGLE"
    a.authenticate.setAuthorizationHeader(transport)
  }

  def getXml(url: String) = {
    val rq = transport.buildGetRequest
    rq.setUrl(url)
    XML.loadString(rq.execute.parseAsString)
  }

  def postXml(url: String)(xml: Node) = {
    val rq = transport.buildPostRequest
    rq.setUrl(url)
    rq.content = new HttpContent {
      def getLength = -1
      def getEncoding = null
      def getType = "application/atom+xml"
      def writeTo(os: OutputStream) {
        os.write(xml.toString.getBytes("UTF-8"))
      }
    }
    rq.execute.parseAsString
  }
}
object GoogleDate {
  private val UTC = TimeZone.getTimeZone("UTC")
  private val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
  df.setCalendar(Calendar.getInstance(UTC))

  def apply(gd: String) = df.parse(gd)
}
trait PrettyDate {
  private val df = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  val updated: Date
  def prettyDate = df.format(updated)
}

case class EntityId(googleId: String) {
  private val R = """.*/([^/]+)""".r
  override def toString = googleId match {
    case R(id) => id
    case another => another
  }
}
object SpreadSheetsUrl {
  def apply(s: String) = "https://spreadsheets.google.com/feeds/" + s
}

class SpreadSheet(val title: String, val updated: Date,
                      val key: EntityId) extends PrettyDate {
  override def toString = "{0} [{1}] {2}" | (key, prettyDate, title)
  def url = SpreadSheetsUrl("worksheets/{0}/private/full" | key)

  class WorkSheet(val title: String, val updated: Date,
                    val id: EntityId, val rowCount: Int) extends PrettyDate {
    override def toString = "{0} [{1}] {2} - {3}" | (id, prettyDate, title, rowCount.toString)
    def listUrl = SpreadSheetsUrl("list/{0}/{1}/private/full" | (key, id))
  }
  object WorkSheet {
    def apply(title: String, updated: String, id: String, rowCount: String) =
      new WorkSheet(title, GoogleDate(updated),
        EntityId(id), Integer.parseInt(rowCount))
  }
  class WorkSheets(val entries: Seq[WorkSheet])

  def load() = {
    new WorkSheets({
      val xml = GoogleClient.getXml(url)
      for {
        entry <- (xml \ "entry")
        title = (entry \ "title").text
        updated = (entry \ "updated").text
        id = (entry \ "id").text
        rowCount = (entry \ "rowCount").text
      } yield WorkSheet(title, updated, id, rowCount)
    })
  }
}
object SpreadSheet {
  def apply(title: String, updated: String, id: String) =
    new SpreadSheet(title, GoogleDate(updated), EntityId(id))
}
class SpreadSheets(val entries: Seq[SpreadSheet])
object SpreadSheets {
  val url = SpreadSheetsUrl("spreadsheets/private/full")

  def load = {
    new SpreadSheets({
      val xml = GoogleClient.getXml(url)
      for {
        entry <- (xml \ "entry")
        title = (entry \ "title").text
        updated = (entry \ "updated").text
        id = (entry \ "id").text
      } yield SpreadSheet(title, updated, id)
    })
  }
}

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

  private def error(msg: String) = {
    Console.err.println(msg)
    System.exit(2)
  }

  try {
    val (user, passwd) = obtainUserAndPassword
    GoogleClient.login(user, passwd)

    val newEntry =
      <entry xmlns="http://www.w3.org/2005/Atom"
          xmlns:gsx="http://schemas.google.com/spreadsheets/2006/extended">
        <gsx:total>15</gsx:total>
        <gsx:active>20</gsx:active>
      </entry>

    for {
      spreadSheet <- SpreadSheets.load.entries.find(_.title == "vbg.report.players").headOption
      workSheet <- spreadSheet.load.entries.headOption
      url = workSheet.listUrl
    } GoogleClient.postXml(url)(newEntry)

    println("OK")
  } catch {
    case e: HttpResponseException =>
      try {
        val info = e.response.parseAs(classOf[ClientLogin.ErrorInfo])
        error(info.error)
      } catch { case _ => error(e.response.parseAsString)}
    case _: ConnectTimeoutException =>
      error("Host connetion is timed out")
    case e: UnknownHostException =>
      error("Unknown host: " + e.getMessage)
  }
}