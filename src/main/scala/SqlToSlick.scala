/**
 * User: bss
 * Date: 6/19/13
 * Time: 12:20 AM
 */

object SqlToSlick {
  def main(args: Array[String]) {

    //val input = io.Source.stdin.getLines().mkString("\n")

    val input =
      """
        | `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
        |  `username` varchar(64) NOT NULL DEFAULT '',
        |  `domain` varchar(64) NOT NULL DEFAULT '',
        |  `password` varchar(25) NOT NULL DEFAULT '',
        |  `email_address` varchar(64) NOT NULL DEFAULT '',
        |  `ha1` varchar(64) NOT NULL DEFAULT '',
        |  `ha1b` varchar(64) NOT NULL DEFAULT '',
        |  `rpid` varchar(64) DEFAULT NULL,
        |  `uuid` int(10) NOT NULL,
        |  `emer_112` varchar(64) NOT NULL DEFAULT '38612370112',
        |  `emer_113` varchar(64) NOT NULL DEFAULT '38612370113',
        |  `emer_1987` varchar(64) NOT NULL DEFAULT '38612370187',
        |  `emer1` varchar(64) NOT NULL,
        |  `emer2` varchar(64) NOT NULL,
        |  `emer3` varchar(64) NOT NULL,
        |  `emer4` varchar(64) NOT NULL,
        |  `emer5` varchar(64) NOT NULL,
        |  `vmail_password` varchar(40) DEFAULT NULL,
        |  `vmail` tinyint(1) NOT NULL DEFAULT '0',
        |  `user` varchar(80) NOT NULL,
        |  `billing_acc_id` mediumint(10) NOT NULL,
        |  `status` int(10) NOT NULL DEFAULT '1',
        |  `enabled` tinyint(1) NOT NULL DEFAULT '1',
        |  `mon` tinyint(1) NOT NULL DEFAULT '0',
        |  `channel_limit` tinyint(1) NOT NULL DEFAULT '1',
        |  `active_calls` int(10) NOT NULL DEFAULT '0'
      """.stripMargin

    val parsed = SqlFieldsParser.parseAll(SqlFieldsParser.fields, input)

    if (parsed.successful) {
      println(parsed.get)
    } else {
      println(parsed)
    }


  }
}
