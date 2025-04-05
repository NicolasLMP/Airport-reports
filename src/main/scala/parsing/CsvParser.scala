package parsing

import java.io.File
import scala.util.Using
import scala.io.StdIn

object CsvParser {
  def parseFile[T](file: File)(fn: String => Either[String, T]): Either[String, List[T]] = {
    Using(io.Source.fromFile(file)) { source =>
      source.getLines().drop(1).toList
        .map(fn)
        .foldLeft(Right(List.empty[T]): Either[String, List[T]]) {
          case (Right(acc), Right(v)) => Right(acc :+ v)
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }
    }.toEither.left.map(_ => "File read error").flatten
  }
}
