package com.mchange.sc.v1

import scala.collection._

package object texttable {
  private val SEP = System.lineSeparator()

  private val DefaultRowFilter : String => Boolean = _ => true

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

    def apply( header : String ) : Column = Column( header = header, mbFieldLength = None, headerFormat = Column.Format.Default, bodyFormat = Column.Format.Default )
  }
  final case class Column( header : String, mbFieldLength : Option[Int], headerFormat : Column.Format, bodyFormat : Column.Format )

  final object Row {
    def apply[T]( t : T ) : Row[T] = Row( t, "" )
  }
  final case class Row[+T]( datum : T, rightSideAnnotation : String )

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

  private def formattedHeader( column : Column, fieldLength : Int ) = String.format( s"%${hjf(column)}${fieldLength}s", recase( column.headerFormat, column.header ) )

  private def formattedRowEntry( column : Column, fieldLength : Int, field : String ) = String.format( s"%${bjf(column)}${fieldLength}s", recase( column.bodyFormat, field ) )

  def extractProduct( product : Product ) : Seq[String] = product.productIterator.map( _.toString ).toSeq

  def appendTable[T]( columns : Seq[Column], extract : T => Seq[String], rowFilter : String => Boolean )( destination : Appendable, rows : Iterable[Row[T]] ) : Unit = {

    val _rowCache = mutable.HashMap.empty[Row[T], Seq[String]]

    val _fieldLengthsCache = mutable.HashMap.empty[Column,Int]

    def _findFieldLength( column : Column ) : Int = {
      column.mbFieldLength match {
        case Some( fieldLength ) => fieldLength
        case None => {
          val index = columns.indexOf( column )
          rows.foldLeft( column.header.length )( ( lastMax, row ) => math.max( lastMax, stringRow( row )(index).length ) )
        }
      }
    }

    def stringRow( row : Row[T] ) =  _rowCache.getOrElseUpdate( row, extract( row.datum ) )

    def fieldLength( column : Column ) : Int = _fieldLengthsCache.getOrElseUpdate( column, _findFieldLength( column ) )

    def appendln( s : String ) = {
      destination.append( s )
      destination.append( SEP )
    }

    val cap = columns.map( column => span( fieldLength( column ) + 2) ).mkString( "+","+","+" )

    val paddedHeaders = columns.map( column => formattedHeader( column, fieldLength( column ) ) )

    val headerLine = paddedHeaders.mkString( "| ", " | ", " |")
    appendln( cap )
    appendln( headerLine )
    appendln( cap )
    rows.foreach { row =>
      val formattedRowEntries = {
        val fields = stringRow( row )
        columns.zip( fields ).map { case ( column, field ) =>
          formattedRowEntry( column, fieldLength( column ), field )
        }
      }
      val datumLine = formattedRowEntries.mkString( "| ", " | ", " |") + row.rightSideAnnotation
      if ( rowFilter( datumLine ) ) {
        appendln( datumLine )
      }
    }
    appendln( cap )
  }

  def appendTable[T]( columns : Seq[Column], extract : T => Seq[String] )( destination : Appendable, rows : Iterable[Row[T]] ) : Unit = {
    appendTable[T]( columns, extract, DefaultRowFilter )( destination, rows )
  }

  def printTable[T]( columns : Seq[Column], extract : T => Seq[String], rowFilter : String => Boolean )( rows : Iterable[Row[T]] ) : Unit = {
    appendTable[T]( columns, extract, rowFilter )( System.out, rows )
  }

  def printTable[T]( columns : Seq[Column], extract : T => Seq[String] )( rows : Iterable[Row[T]] ) : Unit = {
    printTable[T]( columns, extract, DefaultRowFilter )( rows )
  }

  def appendProductTable( columns : Seq[Column], rowFilter : String => Boolean )( destination : Appendable, rows : Iterable[Row[Product]] ) : Unit = {
    appendTable( columns, extractProduct, rowFilter )( destination, rows)
  }

  def appendProductTable( columns : Seq[Column] )( destination : Appendable, rows : Iterable[Row[Product]] ) : Unit = {
    appendProductTable( columns, DefaultRowFilter )( destination, rows )
  }

  def printProductTable( columns : Seq[Column], rowFilter : String => Boolean )( rows : Iterable[Row[Product]] ) : Unit = {
    appendProductTable( columns, rowFilter )( System.out, rows : Iterable[Row[Product]] )
  }

  def printProductTable( columns : Seq[Column] )( rows : Iterable[Row[Product]] ) : Unit = {
    printProductTable( columns, DefaultRowFilter )( rows )
  }
}
