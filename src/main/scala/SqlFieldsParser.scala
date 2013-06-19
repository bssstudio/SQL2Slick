import util.parsing.combinator.JavaTokenParsers

/**
 * User: bss
 * Date: 6/18/13
 * Time: 10:28 PM
 */

object SqlFieldsParser extends JavaTokenParsers {

  def fields: Parser[String] = repsep(createDefinition,",") ^^ {
    case e :: listE => listE.fold(e) { (a,b) =>
      a + "\n" + b
    }
    case Nil => sys.error("assertion fail.")
  }

  def createDefinition: Parser[String] = """`?[a-zA-Z0-9_-]+`?""".r ~ columnDefinition ^^ {
    case columnName ~ columnDef => {
      val colName = columnName.filter((c:Char) => c!='`')
      val (dataType, nullity, default, autoinc) = columnDef
      "val "+colName+" = column["+dataType+"]("+'"'+colName+'"' +
      nullity.fold("")( nullity => ", "+(if (nullity) { "O.Nullable" } else { "O.NotNull" }) ) +
      autoinc.fold("")( ainc => ", O.AutoInc") +
      default.fold("") { defa =>
        ", O.Default("+
        (dataType match {
          case "String" => '"'+defa+'"'
          case _ => defa
        })+")"
      }+
      ")"
    }
  }

  def columnDefinition = dataType ~ nullnotnull.?  ~ default.?  ~ autoinc.?   ^^ {
    case dataType ~ nullity ~ default ~ autoinc => (dataType, nullity, default, autoinc)
    case _ => ("", None, None, None)
  }


  def autoinc = "AUTO_INCREMENT" ^^ {
    case _ => true
  }

  def nullnotnull = ( "NULL" | ("NOT" ~ "NULL") ) ^^ {
    case "NULL" => true
    case "NOT" ~ "NULL" => false
  }

  def default = ( "DEFAULT" ~ """`?'?[a-zA-Z0-9_-]*'?`?""".r ) ^^ {
    case "DEFAULT" ~ defval => defval.dropWhile(c => c=='\'' || c=='`').reverse.dropWhile(c => c=='\'' || c=='`').reverse
  }

  def dataType = INTEGER  | BIGINT | VARCHAR | TINYINT | MEDIUMINT

  def INTEGER = ("INTEGER" | "Int" | "int" | "integer" | "Integer") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }

  def BIGINT = "BIGINT" ~ ("(" ~ wholeNumber ~ ")").?  ^^ {
    case _ => "Long"
  }

  def VARCHAR = ("VARCHAR" | "varchar") ~ ("(" ~ wholeNumber ~ ")").?  ^^ {
    case _ => "String"
  }

  def TINYINT = ("TINYINT" | "tinyint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }

  def MEDIUMINT = ("MEDIUMINT" | "mediumint") ~ ("(" ~ wholeNumber ~ ")").? ~ ("unsigned" | "UNSIGNED").? ~ ("ZEROFILL").?  ^^ {
    case _ => "Int"
  }
}
