import org.scalatest.FunSuite
import si.bss.tools.sql2slick.{SqlFieldsParser, SlickGenerator}

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
    val fields = parsed.get.filter(_.isDefined).map(_.get)
    fields.foreach { f =>
      println(SlickGenerator.genColumnDef(f))
    }

  }

  test("Parsing of a real life example") {

    val str =
      """
        | `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        | `alias_username` varchar(64) NOT NULL DEFAULT '',
        | `alias_domain` varchar(64) NOT NULL DEFAULT '',
        | `username` varchar(64) NOT NULL DEFAULT '' COMMENT 'username of the subscriber',
        | `domain` varchar(64) NOT NULL DEFAULT ''
      """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.fields,str)
    println(parsed)
    val fields = parsed.get.filter(_.isDefined).map(_.get)
    fields.foreach { f =>
      println(SlickGenerator.genColumnDef(f))
    }

  }

  test("Parsing of a real life example - with garbage") {

    val str =
      """ `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        |  `alias_username` varchar(64) NOT NULL DEFAULT '',
        |  `alias_domain` varchar(64) NOT NULL DEFAULT '',
        |  `username` varchar(64) NOT NULL DEFAULT '',
        |  `domain` varchar(64) NOT NULL DEFAULT '',
        |  PRIMARY KEY (`id`),
        |  UNIQUE KEY `alias_idx` (`alias_username`,`alias_domain`),
        |  KEY `target_idx` (`username`,`domain`)
      """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.fields,str)
    println(parsed)
    val fields = parsed.get.filter(_.isDefined).map(_.get)
    fields.foreach { f =>
      println(SlickGenerator.genColumnDef(f))
    }
  }

  test("Parsing of whole table def") {
    val str =
    """
      |CREATE TABLE IF NOT EXISTS `dbaliases` (
      |  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
      |  `alias_username` varchar(64) NOT NULL DEFAULT '',
      |  `alias_domain` varchar(64) NOT NULL DEFAULT '',
      |  `username` varchar(64) NOT NULL DEFAULT '',
      |  `domain` varchar(64) NOT NULL DEFAULT '',
      |  PRIMARY KEY (`id`),
      |  UNIQUE KEY `alias_idx` (`alias_username`,`alias_domain`),
      |  KEY `target_idx` (`username`,`domain`)
      |) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=87522 ;
    """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.table,str)
    assert(parsed.successful)
    println(parsed)
  }
}
