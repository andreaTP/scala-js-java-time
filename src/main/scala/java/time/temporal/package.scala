package java.time

package object temporal {
  private[time] def toScreamingSnakeCase(s: String): String = {
    // TODO: replaceAll with reg-ex is not working correctly in scala native
    val s1 = {
      var tmp = s
      for (l <- ('A' to 'Z'))
        tmp = tmp.replaceAll(l.toString, "_" + l)
      
      tmp.toUpperCase
    }
    val s2 = s1.replace("AM_PM", "AMPM")
    if (s2.startsWith("_")) s2.tail else s2
  }
}
