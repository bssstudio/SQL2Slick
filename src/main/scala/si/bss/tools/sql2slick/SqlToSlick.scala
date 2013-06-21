package si.bss.tools.sql2slick

/**
 * User: bss
 * Date: 6/19/13
 * Time: 12:20 AM
 */

import StringCamelImplicits._

object SqlToSlick {
  def main(args: Array[String]) {

    println("--------------------------------------------------------------")
    println(" Please paste your SQL table definition (followed by CTRL+D):")
    println("--------------------------------------------------------------")

    val input = io.Source.stdin.getLines().mkString("\n")

    println("--------------------------------------------------------------")

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.table, input)
    if (parsed.successful) {

      val (tableName, fields) = parsed.get
      val className = tableName.toCapitalizedCamelCaseIdent

      println()

      println("--------------------------------------------------------------")
      println(" Parsed successfully: "+tableName)
      println(" Please copy the code below:")
      println("--------------------------------------------------------------")

      println()

      println(SlickGenerator.genCaseClass(className, fields))
      println()

      println()

      println(SlickGenerator.genMappedTable(tableName,className,fields))
      println()

    } else {
      println()

      println("--------------------------------------------------------------")
      println(" Parsing failed.")
      println("--------------------------------------------------------------")

      println()
      println(parsed)
    }


  }
}
