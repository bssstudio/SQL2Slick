package si.bss.tools.sql2slick

import scala.Predef._
import util.parsing.combinator.JavaTokenParsers

/**
 * User: bss
 * Date: 6/18/13
 * Time: 10:28 PM
 */

case class SqlField(columnName: String, dataType: String, nullity: Option[Boolean], default: Option[String], autoInc: Boolean)

object SqlFieldsParser extends JavaTokenParsers {

  def table = tableHeader ~ "(" ~ fields <~ ")" ~ ".*".r.? ^^ {
    case tableName ~ _ ~ fields => (tableName.filter((c:Char) => c!='`'), fields.filter(_.isDefined).map(_.get))
  }

  def tableHeader = "CREATE" ~ "TEMPORARY".? ~ "TABLE" ~ ("IF" ~ "NOT" ~ "EXISTS").? ~> """`?[a-zA-Z0-9_-]+`?""".r

  def fields = repsep(createDefinition | ignoreFld ,",")

  def createDefinition: Parser[Option[SqlField]] = """`?[a-zA-Z0-9_-]+`?""".r ~ columnDefinition ^^ {
    case columnName ~ columnDef => {
      val (dataType, nullity, default, autoinc) = columnDef
      Some(SqlField(columnName.filter((c:Char) => c!='`'), dataType, nullity, default, autoinc.getOrElse(false)))
    }
  }

  def columnDefinition = dataType ~ nullnotnull.?  ~ default.?  ~ autoinc.? ~ ignore.?  ^^ {
    case dataType ~ nullity ~ default ~ autoinc ~ _ => (dataType, nullity, default, autoinc)
    case _ => ("", None, None, None)
  }


  def autoinc = "AUTO_INCREMENT" ^^ {
    case _ => true
  }

  def nullnotnull = "NOT".? <~ "NULL" ^^ { _.isEmpty }

  def default = "DEFAULT" ~> """`?'?[a-zA-Z0-9_-]+'?`?""".r  ^^ {
    _.dropWhile(c => c=='\'' || c=='`').reverse.dropWhile(c => c=='\'' || c=='`').reverse
  }

  def ignoreFld: Parser[Option[SqlField]] = rep(ignoreOne) ^^ { _ => None }
  def ignore: Parser[String] = rep(ignoreOne) ^^ { _ => "" }
  def ignoreOne: Parser[String] = ("'[^']*'".r | "`[^`]*`".r  | """[a-zA-Z0-9_-]+""".r | ignoreParenthesis) ^^ { _ => "" }
  def ignoreParenthesis: Parser[String] = "(" ~ repsep(ignore,",") ~ ")" ^^ { _ => "" }


  def dataType = INTEGER  | BIGINT | VARCHAR | TINYINT | MEDIUMINT | DOUBLE | DECIMAL | TEXT | BLOB | BINARY | VARBINARY | DATETIME

  def INTEGER = ("INTEGER" | "INT" | "Int" | "int" | "integer" | "Integer") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {_ => "Int"}

  def TINYINT = ("TINYINT" | "tinyint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {_ => "Int"}

  def BIGINT = ("BIGINT" | "bigint") ~ ("(" ~ wholeNumber ~ ")").?  ^^ { _ => "Long"}

  def MEDIUMINT = ("MEDIUMINT" | "mediumint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ { _ => "Int"}

  def VARCHAR = ("VARCHAR" | "varchar" | "CHAR" | "char") ~ ("(" ~ wholeNumber ~ ")").? ~ ("CHARACTER" ~ "SET" ~ """[a-zA-Z0-9_-]+""".r).? ~ ("COLLATE" ~ """[a-zA-Z0-9_-]+""".r).? ^^ { _ => "String"}

  def TEXT = ("TEXT" | "text" | "MEDIUMTEXT" | "mediumtext" | "LONGTEXT" | "longtext") ~ ("BINARY" | "binary").? ~ ("CHARACTER" ~ "SET" ~ """[a-zA-Z0-9_-]+""".r).? ~> ("COLLATE" ~ """[a-zA-Z0-9_-]+""".r).? ^^ {
    case Some(_)  => "Array[Byte]"
    case None => "String"
  }

  def DOUBLE = ("DOUBLE" | "double" | "FLOAT" | "float") ~ ("(" ~ wholeNumber ~"," ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ { _ => "Double"}

  def DECIMAL = ("DECIMAL" | "decimal" | "NUMERIC" | "numeric") ~ ("(" ~ wholeNumber ~"," ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ { _ => "BigDecimal"}

  def BLOB = ("TINYBLOB" | "BLOB" | "MEDIUMBLOB" | "LONGBLOB" | "tinyblob" | "blob" | "mediumblob" | "longblob") ^^ { _ => "Array[Byte]"}

  def BINARY = ("BINARY" | "binary") ~ ("(" ~ wholeNumber ~ ")").? ^^ {_ => "Array[Byte]"}

  def VARBINARY = ("VARBINARY" | "varbinary") ~ ("(" ~ wholeNumber ~ ")") ^^ { _ => "Array[Byte]"}

  def DATETIME = ("DATETIME" | "datetime" | "DATE" | "date" | "TIME" | "time" | "TIMESTAMP" | "timestamp") ^^ { _ => "DateTime" }
}
