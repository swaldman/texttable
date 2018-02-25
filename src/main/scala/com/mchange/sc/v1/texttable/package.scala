package com.mchange.sc.v1

package object texttable {
  private val SEP = System.lineSeparator()

  private def span( len : Int ) = (0 until len).map(_ => "-").mkString

  final object Column {
    object Format {
      object Justification {
        case object Left  extends Justification
        case object Right extends Justification
      }
      sealed trait Justification

      object Case {
        case object Upper extends Case
        case object Lower extends Case
        case object Mixed extends Case
      }
      sealed trait Case

      val Default = Format( Justification.Left, Case.Mixed )
    }
    final case class Format( justification : Format.Justification, `case` : Format.Case )
  }
  final case class Column( header : String, fieldLength : Int, headerFormat : Column.Format = Column.Format.Default, bodyFormat : Column.Format = Column.Format.Default )

  final case class Row[T]( datum : T, rightSideAnnotation : String )

  private def justificationFlag( format : Column.Format ) : String = if ( format.justification == Column.Format.Justification.Left ) "-" else ""
  private def hjf( column : Column ) = justificationFlag( column.headerFormat ) // header justification flag
  private def bjf( column : Column ) = justificationFlag( column.bodyFormat )   // body justification flag

  private def recase( format : Column.Format, value : String ) = {
    import Column.Format.Case._

    format.`case` match {
      case Upper => value.toUpperCase
      case Lower => value.toLowerCase
      case Mixed => value
    }
  }

  private def formattedHeader( column : Column ) = String.format( s"%${hjf(column)}${column.fieldLength}s", recase( column.headerFormat, column.header ) )

  private def formattedRowEntry( column : Column, field : String ) = String.format( s"%${bjf(column)}${column.fieldLength}s", recase( column.bodyFormat, field ) )

  private val freTupled = (formattedRowEntry _).tupled

  def extractProduct( product : Product ) : Seq[String] = product.productIterator.map( _.toString ).toSeq

  def appendTable[T]( columns : Seq[Column], extract : T => Seq[String] )( destination : Appendable, rows : Seq[Row[T]] ) : Unit = {

    def appendln( s : String ) = {
      destination.append( s )
      destination.append( SEP )
    }

    val cap = columns.map( column => span( column.fieldLength + 2) ).mkString( "+","+","+" )

    val paddedHeaders = columns.map( formattedHeader )

    val headerLine = paddedHeaders.mkString( "| ", " | ", " |")
    appendln( cap )
    appendln( headerLine )
    appendln( cap )
    rows.foreach { row =>
      val formattedRowEntries = {
        val fields = extract( row.datum )
        columns.zip( fields ).map( freTupled )
      }
      val datumLine = formattedRowEntries.mkString( "| ", " | ", " |") + row.rightSideAnnotation
      appendln( datumLine )
    }
    appendln( cap )
  }

  def appendTable( columns : Seq[Column] )( destination : Appendable, rows : Seq[Row[Product]] ) : Unit = {
    appendTable( columns, extractProduct )( destination, rows)
  }

  def printTable[T]( columns : Seq[Column], extract : T => Seq[String] )( rows : Seq[Row[T]] ) : Unit = {
    appendTable[T]( columns, extract )( System.out, rows )
  }

  def printTable( columns : Seq[Column] )( rows : Seq[Row[Product]] ) : Unit = {
    appendTable( columns )( System.out, rows : Seq[Row[Product]] )
  }
}
