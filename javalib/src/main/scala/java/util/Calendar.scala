package java.util

import java.io.Serializable

class Calendar(zone: TimeZone, aLocale: Locale)
    extends Serializable
    with Cloneable
    with Comparable[Calendar] {
  def get(field: Int): Int = ???

  def compareTo(anotherCalendar: Calendar): Int = ???

  def getFirstDayOfWeek(): Int = ???

  def getMinimalDaysInFirstWeek(): Int = ???

  def getTime(): Date = ???

  def getTimeInMillis(): Long = ???

  def getTimeZone(): TimeZone = ???

  def setTime(date: Date): Unit = ???
}

object Calendar {
  def getInstance(locale: Locale): Calendar = ???

  val YEAR: Int = 0
  val MONTH: Int = 0
  val DAY_OF_MONTH: Int = 0
  val DAY_OF_YEAR: Int = 0
  val DAY_OF_WEEK: Int = 0
  val AM_PM: Int = 0
  val HOUR: Int = 0
  val HOUR_OF_DAY: Int = 0
  val MINUTE: Int = 0
  val SECOND: Int = 0
  val MILLISECOND: Int = 0
  val ZONE_OFFSET: Int = 0
}
