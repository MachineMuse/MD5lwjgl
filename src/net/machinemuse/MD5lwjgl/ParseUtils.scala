package net.machinemuse.MD5lwjgl

import scala.io.Source

/**
 * Author: MachineMuse (Claire Semple)
 * Created: 3:59 PM, 7/2/13
 */
object ParseUtils {
  // Search for '//' and remove everything after
  def trimComments(line: String) = {
    line.substring(0, line.indexOf("//")).trim
  }

  def toRegex(pattern:String) = {
    pattern.
      replaceAll("number", "([\\d\\.]+)").
      replaceAll("word", "(\\w+)").
      r
  }
}