package com.katlex.reporter

import com.google.api.client.googleapis.GoogleTransport
import com.google.api.client.googleapis.auth.clientlogin.ClientLogin
import com.google.api.client.http.HttpContent
import java.io.OutputStream
import xml.{Node, XML, NodeBuffer}
import java.text.SimpleDateFormat
import java.util.{Calendar, TimeZone}
import java.util.Date

object StringSubstitution {
  implicit def stringSubstitute(s: String) = new Object {
    def |(substitutions: Any*) = {
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
object Entry {
  def apply(values: (Any, Any)*) =
    <entry xmlns="http://www.w3.org/2005/Atom"
          xmlns:gsx="http://schemas.google.com/spreadsheets/2006/extended">
      {
        var buffer = new NodeBuffer
        for {
          (column, value) <- values
        } buffer ++= XML.loadString(
          "<gsx:{0}>{1}</gsx:{0}>" | (column, value))
        buffer
      }
    </entry>
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
    override def toString = "{0} [{1}] {2} - {3}" |
                            (id, prettyDate, title, rowCount)
    def listUrl = SpreadSheetsUrl("list/{0}/{1}/private/full" | (key, id))
    def addLine(values: (Any, Any)*) =
      GoogleClient.postXml(listUrl)(Entry(values: _*))

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