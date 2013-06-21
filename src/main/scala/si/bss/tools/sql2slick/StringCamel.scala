package si.bss.tools.sql2slick

/**
 * User: bss
 * Date: 6/19/13
 * Time: 12:41 PM
 */


object StringCamelImplicits {
  implicit class StringCamel(str: String) {

    def toCamelCase: String = {
      val parts = str.split(Array(' ', '_','-'))
      parts.tail.fold(parts.head) { (a,b) =>
        a + b.toLowerCase.capitalize
      }
    }

    def toCamelCaseIdent: String = {
      val camel = toCamelCase
      camel match {
        case "class" => "aClass"
        case "case" => "aCase"
        case "type" => "typ"
        case _ => camel
      }
    }

    def toCapitalizedCamelCase: String = {
      toCamelCase.capitalize
    }

    def toCapitalizedCamelCaseIdent: String = {
      toCamelCaseIdent.capitalize
    }

  }
}