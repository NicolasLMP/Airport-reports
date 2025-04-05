package processing

import models._
import parsing.CsvParser
import java.io.File
import scala.concurrent.{Future, ExecutionContext}
import scala.util._ // Added import for Either type

class DataLoader(implicit ec: ExecutionContext) {
  def loadDataParallel(
    countriesFile: File,
    airportsFile: File,
    runwaysFile: File
  ): Future[Either[String, (List[Country], List[Airport], List[Runway])]] = {
    val countriesF = Future(CsvParser.parseFile(countriesFile)(Country.from))
    val airportsF = Future(CsvParser.parseFile(airportsFile)(Airport.from))
    val runwaysF = Future(CsvParser.parseFile(runwaysFile)(Runway.from))
    
    for {
      c <- countriesF
      a <- airportsF
      r <- runwaysF
    } yield {
      for {
        countries <- c
        airports <- a
        runways <- r
      } yield (countries, airports, runways)
    }
  }
}