import org.scalatest.FunSuite

/**
 * User: bss
 * Date: 6/18/13
 * Time: 10:29 PM
 */

class SqlFieldsParserTest extends FunSuite {

  test("Parsing of a simple create table field") {

    val str =
      """
        | `alias_username` varchar(64) NOT NULL DEFAULT '',
        | `alias_username` varchar(64) NOT NULL DEFAULT '123' AUTO_INCREMENT,
        | `alias_username` varchar(64) NOT NULL,
        | testField INTEGER(12) NOT NULL DEFAULT 0
      """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.fields,str)
    println(parsed)

  }

  test("Parsing of a real life example") {

    val str =
      """
        | `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        | `alias_username` varchar(64) NOT NULL DEFAULT '',
        | `alias_domain` varchar(64) NOT NULL DEFAULT '',
        | `username` varchar(64) NOT NULL DEFAULT '',
        | `domain` varchar(64) NOT NULL DEFAULT ''
      """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.fields,str)
    println(parsed)

  }

}
