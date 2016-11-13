// `@java.util.regex.Pattern$::compile_class.java.lang.String_class.java.util.regex.Pattern`
// `@java.util.regex.Pattern$`
// `@java.util.regex.Pattern::split_trait.java.lang.CharSequence_class.ssnr.ObjectArray`
// `@java.util.regex.Pattern`


package java.util.regex

object Pattern {
  def compile(regex: String): Pattern = {
    println(regex)
    new Pattern
  }

  def matches(regex: String, input: CharSequence): Boolean = ???
}

class Pattern {
  def split(seq: java.lang.CharSequence): Array[String] = {
    println(seq.toString())
    Array()
  }

  def split(input: CharSequence, limit: Int): Array[String] = ???

  def matcher(seq: CharSequence): Matcher = ???
}
