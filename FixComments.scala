import java.io.{FileWriter, FileOutputStream, File}
import java.text.SimpleDateFormat
import java.util.Date
import scala.language.postfixOps
import scala.xml.{Node, Elem}

val issuesFile = new File("issues.csv")
val commentsFile = new File("withcomments.xml")
val resultFile = new File("result.csv")
// Thu, 20 Aug 2015 13:28:57 -0400
val originalCommentDateFormat = new SimpleDateFormat("EEE, dd MMM yyyy hh:mm:ss ZZZZZ")
// 18/Aug/15 10:54 AM
val targetCommentDateFormat = new SimpleDateFormat("dd/MMM/yy hh:mm aa")


var issuesAsLines = scala.io.Source.fromFile(issuesFile).getLines().toList
var header = issuesAsLines.head
/* skip headers */
issuesAsLines = issuesAsLines.tail
/* some issues take up several lines */
issuesAsLines = reformatLines(issuesAsLines)
val csvIssues = issuesAsLines map issueFromRow
// change as needed
if (csvIssues.size != 109) {
  throw new Exception("Inconsistent issue count")
}

var xmlItems = scala.xml.XML.loadFile(commentsFile) \\ "item"
val xmlIssues = xmlItems map issueFromItem

/* matching on key */
val matchedIssues: List[(CsvIssue, XmlIssue)] = csvIssues map { csv =>
  val xml = xmlIssues find {_.id == csv.id} get;
  (csv, xml)
}

/* the largest count of filled columns */
val maxColumns = csvIssues map {_.columnCount} max
val maxComments = xmlIssues map {_.comments size} max
val mergedIssues = mergeIssues(matchedIssues, maxColumns)
header = appendHeader(header, maxComments)

val writer = new FileWriter(resultFile)
writer write header
writer write "\n"
mergedIssues foreach {issue =>
  writer write issue.toString
  writer write "\n"
}
writer close()

/* Functions */

def issueFromRow(row: String): CsvIssue =
{
  val issue = new CsvIssue
  issue.columns = row split "\t" map {_.trim} toList;
  if (issue.columnCount < 2) {
    throw new Exception("CSV improperly formatted")
  }
  issue.id = issue.columns(1)
  issue
}

def issueFromItem(item: Node): XmlIssue =
{
  val issue = new XmlIssue
  issue.id = {item \ "key"} text
  val comments = {item \ "comments" \\ "comment"} map commentFromCommentNode
  issue.comments = comments toList;
  issue
}

def commentFromCommentNode(node: Node): Comment =
{
  val author = node \@ "author"
  val stringDate = node \@ "created"
  val body = node.text
  val date = originalCommentDateFormat.parse(stringDate)
  new Comment(author, date, body)
}

def reformatLines(lines: List[String]): List[String] =
{
  var newList = List[String]()

  var buffer = ""
  lines foreach { l =>
    val isIssueStart = if (l.length >= 13) {
      val lineStart = l substring(0, 13)
      lineStart.contains("PhenoTips") && lineStart.contains("PT")
    } else
    {
      false
    }
    if (isIssueStart) {
      if (buffer nonEmpty) {
        newList :+= buffer
        buffer = ""
      }
    }
    buffer += l
  }

  newList :+= buffer
  newList
}

def appendHeader(header: String, padBy: Int): String = {
  val append = (0 until padBy) map {i => "Comment" + i} reduce (_ + "\t" + _)
  header + append
}

def mergeIssues(issues: List[(CsvIssue, XmlIssue)], fillToColumn: Int): Iterable[CsvIssue] = issues map {pair =>
  val csv = pair._1
  csv.columns padTo (fillToColumn, "\t")
  csv.columns ++= pair._2.comments map {_.toString}
  csv
}

/* Classes */

class CsvIssue() extends Issue
{
  var columns: List[String] = List()

  def columnCount = columns.size

  override def toString: String = {
    columns map {"\"" + _ + "\""} reduce (_ + "\t" + _)
  }
}

class XmlIssue() extends Issue
{
  var comments = List[Comment]()
}

case class Comment(author: String, date: Date, body: String) {
  override def toString: String = targetCommentDateFormat.format(date) + ";" + author + ";" + body
}

trait Issue
{
  var id: String = ""
}
