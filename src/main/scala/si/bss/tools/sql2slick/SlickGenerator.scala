package si.bss.tools.sql2slick

import StringCamelImplicits._

/**
 * User: bss
 * Date: 6/19/13
 * Time: 12:33 PM
 */

object SlickGenerator {

 val dateTimeImports =
   """import org.joda.time.DateTime
     |""".stripMargin

  val dateTimeMapperImports =
    """import slick.lifted.MappedTypeMapper
      |import java.sql.{Timestamp, Date}
      |import org.joda.time.DateTime
      |""".stripMargin

  val dateTimeMapper =
    """
      |  implicit def dateTimeTypeMapper =
      |    MappedTypeMapper.base[DateTime, Timestamp](
      |      dt => new Timestamp(dt.getMillis),
      |      ts => new DateTime(ts.getTime)
      |    )
      |""".stripMargin

  def fieldsContainDateTime(fields: Seq[SqlField]): Boolean = {
    fields.foldLeft(false) { (containsDateTime,field) =>
      (containsDateTime || (field.dataType == "DateTime"))
    }
  }

  def genColumnDef(field: SqlField) = {

    "val "+field.columnName.toCamelCaseIdent+" = column["+field.dataType+"]("+'"'+field.columnName+'"' +
      field.nullity.fold("")( nullity => ", "+(if (nullity) { "O.Nullable" } else { "O.NotNull" }) ) +
      (if (field.autoInc) {", O.AutoInc"} else {""}) +
      field.default.fold("") { defa =>
        ", O.Default("+
          (field.dataType match {
            case "String" => '"'+defa+'"'
            case _ => defa
          })+")"
      }+
      ")"
  }


  private def isFieldOptional(field: SqlField): Boolean = {
    if (field.autoInc) {
      true
    } else {
      false
    }
  }

  def genStarProjection(fields: Seq[SqlField]) = {
    "def * = " +
    (
      fields.map { field =>
        field.columnName.toCamelCaseIdent +
        (if (isFieldOptional(field)) {
          ".?"
        } else {
          ""
        })
      }.mkString(" ~\n  ")
    )
  }

  def genCaseClass(className: String, fields: Seq[SqlField]) = {
    "/* Generated case class */\n" +
    (if (fieldsContainDateTime(fields)) {
      dateTimeImports + "\n"
    } else {
      ""
    })+
    "case class "+className+"(" +
    (
      fields.map { field =>
        field.columnName.toCamelCaseIdent + ": "+
        (if (isFieldOptional(field)) {
          "Option["+field.dataType+"]"
        } else {
          field.dataType
        })
      }.mkString(",\n  ")
    ) +
    ")"
  }

  def genMappedStarProjection(className: String, fields: Seq[SqlField]): String = {
    genStarProjection(fields) + " <> (" +
    " table => " + className + "(" +
    (
      (1 to fields.length).map(i => "table._"+i).mkString(", \n      ")
    ) +
    "),"+
    "\n\n" +
    (
      "    (obj: "+className+") => Some(("+
      (
        fields.map { field =>
          "obj."+field.columnName.toCamelCaseIdent
        }.mkString(", \n      ")
      ) +
      "))"
    ) +
    "\n  )"
  }


  def genMappedTable(tableName: String, className: String, fields: Seq[SqlField]): String = {
    "/* Generated slick table class */\n" +
    (if (fieldsContainDateTime(fields)) {
      dateTimeMapperImports + "\n"
    } else {
      ""
    })+
    "object "+className+"Table extends Table["+className+"]("+'"'+tableName+'"'+") {\n"+
    (if (fieldsContainDateTime(fields)) {
      "  " + dateTimeMapper + "\n"
    } else {
      ""
    })+
    "  " + fields.map(genColumnDef).mkString("\n  ")+
    "\n\n"+
    "  " + genMappedStarProjection(className, fields).lines.mkString("\n  ")+
    "\n"+
    "}"
  }
}
