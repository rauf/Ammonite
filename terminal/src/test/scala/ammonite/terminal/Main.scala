package ammonite.terminal

import java.io.OutputStreamWriter


import ammonite.terminal.GUILikeFilters.SelectionFilter
import ammonite.terminal.LazyList.~:

import scala.annotation.tailrec



// Test Unicode:  漢語;𩶘da
/**
 * A "full" test terminal including all readline-style functionality
 * included as filters, but without any Scala/Ammonite specific logic
 * at all. Provides a minimal environment for manual testing
 */
object Main{

  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    var history = List.empty[String]
    val selection = GUILikeFilters.SelectionFilter(indent = 4)
    def multilineFilter: TermCore.Filter = {
      case TermState(13 ~: rest, b, c, _) if b.count(_ == '(') != b.count(_ == ')') =>
        BasicFilters.injectNewLine(b, c, rest)
    }
    val reader = new java.io.InputStreamReader(System.in)
    rec()
    @tailrec def rec(): Unit = {
      val historyFilter = new HistoryFilter(
        () => history.toVector, Console.BLUE, AnsiNav.resetForegroundColor)
      TermCore.readLine(
        Console.MAGENTA + (0 until 10).mkString + "\n@@@ " + Console.RESET,
        reader,
        new OutputStreamWriter(System.out),
        historyFilter orElse
        multilineFilter orElse
        selection orElse
        BasicFilters.tabFilter(4) orElse
        GUILikeFilters.altFilter orElse
        GUILikeFilters.fnFilter orElse
        ReadlineFilters.navFilter orElse
        ReadlineFilters.CutPasteFilter() orElse
//        Example multiline support by intercepting Enter key
        BasicFilters.all,
        // Example displayTransform: underline all non-spaces
        displayTransform = (buffer, cursor) => {
          // underline all non-blank lines

          def hl(b: Vector[Char]): Vector[Char] = b.flatMap{
            case ' ' => " "
            case '\n' => "\n"
            case c => Console.UNDERLINED + c + Console.RESET
          }
          // and highlight the selection
          val ansiBuffer = Ansi.Str.parse(hl(buffer))
          val (newBuffer, cursorOffset) = SelectionFilter.mangleBuffer(
            selection, ansiBuffer, cursor, Ansi.Reversed
          )
          val newNewBuffer = HistoryFilter.mangleBuffer(
            historyFilter, newBuffer, cursor,
            Ansi.Green
          )

          (newNewBuffer, cursorOffset)
        }
      ) match {
        case None => println("Bye!")
        case Some(s) =>
          history = s :: history
          println(s)
          rec()
      }
    }
  }
}

