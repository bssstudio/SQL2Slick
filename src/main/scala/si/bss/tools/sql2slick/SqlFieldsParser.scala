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


  def table = tableHeader ~ "(" ~ fields ~ ")" ~ ".*".r.? ^^ {
    case tableName ~ _ ~ fields ~ _  ~ _ => (tableName.filter((c:Char) => c!='`'), fields.filter(_.isDefined).map(_.get))
  }

  def tableHeader = "CREATE" ~ "TEMPORARY".? ~ "TABLE" ~ ("IF" ~ "NOT" ~ "EXISTS").? ~ """`?[a-zA-Z0-9_-]+`?""".r ^^ {
    case _ ~ _ ~ _ ~ _ ~ tableName => tableName
  }

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

  def nullnotnull = ( "NULL" | ("NOT" ~ "NULL") ) ^^ {
    case "NULL" => true
    case "NOT" ~ "NULL" => false
  }

  def default = ( "DEFAULT" ~ """`?'?[a-zA-Z0-9_-]+'?`?""".r ) ^^ {
    case "DEFAULT" ~ defval => defval.dropWhile(c => c=='\'' || c=='`').reverse.dropWhile(c => c=='\'' || c=='`').reverse
  }

  def ignoreFld: Parser[Option[SqlField]] = rep(ignoreOne) ^^ { case _ => None }
  def ignore: Parser[String] = rep(ignoreOne) ^^ { case _ => "" }
  def ignoreOne: Parser[String] = ("'[^']*'".r | "`[^`]*`".r  | """[a-zA-Z0-9_-]+""".r | ignoreParenthesis) ^^ { case _ => "" }
  def ignoreParenthesis: Parser[String] = "(" ~ repsep(ignore,",") ~ ")" ^^ { case _ => "" }


  def dataType = INTEGER  | BIGINT | VARCHAR | TINYINT | MEDIUMINT | DOUBLE | DECIMAL | TEXT | BLOB | BINARY | VARBINARY | DATETIME

  def INTEGER = ("INTEGER" | "INT" | "Int" | "int" | "integer" | "Integer") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }

  def TINYINT = ("TINYINT" | "tinyint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }

  def BIGINT = ("BIGINT" | "bigint") ~ ("(" ~ wholeNumber ~ ")").?  ^^ {
    case _ => "Long"
  }

  def MEDIUMINT = ("MEDIUMINT" | "mediumint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }

  def VARCHAR = ("VARCHAR" | "varchar" | "CHAR" | "char") ~ ("(" ~ wholeNumber ~ ")").? ~ ("CHARACTER" ~ "SET" ~ """[a-zA-Z0-9_-]+""".r).? ~ ("COLLATE" ~ """[a-zA-Z0-9_-]+""".r).? ^^ {
    case _ => "String"
  }

  def TEXT = ("TEXT" | "text" | "MEDIUMTEXT" | "mediumtext" | "LONGTEXT" | "longtext") ~ ("BINARY" | "binary").? ~ ("CHARACTER" ~ "SET" ~ """[a-zA-Z0-9_-]+""".r).? ~ ("COLLATE" ~ """[a-zA-Z0-9_-]+""".r).? ^^ {
    case _ ~ Some(_)  => "Array[Byte]"
    case _ ~ None => "String"
  }

  def DOUBLE = ("DOUBLE" | "double" | "FLOAT" | "float") ~ ("(" ~ wholeNumber ~"," ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Double"
  }

  def DECIMAL = ("DECIMAL" | "decimal" | "NUMERIC" | "numeric") ~ ("(" ~ wholeNumber ~"," ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "BigDecimal"
  }

  def BLOB = ("TINYBLOB" | "BLOB" | "MEDIUMBLOB" | "LONGBLOB" | "tinyblob" | "blob" | "mediumblob" | "longblob") ^^ {
    case _ => "Array[Byte]"
  }

  def BINARY = ("BINARY" | "binary") ~ ("(" ~ wholeNumber ~ ")").? ^^ {
    case _ ~ size => "Array[Byte]"
  }

  def VARBINARY = ("VARBINARY" | "varbinary") ~ ("(" ~ wholeNumber ~ ")") ^^ {
    case _ => "Array[Byte]"
  }

  def DATETIME = ("DATETIME" | "datetime" | "DATE" | "date" | "TIME" | "time" | "TIMESTAMP" | "timestamp") ^^ {
    case _ => "DateTime"
  }
}
