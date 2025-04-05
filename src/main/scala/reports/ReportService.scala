package reports

import database.DatabaseService
import scala.concurrent.{ExecutionContext, Future}

class ReportService(dbService: DatabaseService)(implicit ec: ExecutionContext) {
  def generateReports(): Future[List[String]] = {
    for {
      topAirports <- topCountriesByAirports()
      bottomAirports <- bottomCountriesByAirports()
      surfaces <- runwaySurfacesByCountry()
      runwayLatitudes <- topRunwayLatitudes()
      countryNames <- getCountryNames((topAirports ++ bottomAirports).map(_._1).toSet ++ surfaces.keys)
    } yield {
      val topAirportReport = formatTopAirportsReport(topAirports, countryNames)
      val bottomAirportReport = formatBottomAirportsReport(bottomAirports, countryNames)
      val surfaceReport = formatRunwaySurfacesReport(surfaces, countryNames)
      val latitudeReport = formatRunwayLatitudesReport(runwayLatitudes)
      
      List(topAirportReport, bottomAirportReport, surfaceReport, latitudeReport)
    }
  }
  
  def generateTopAirportsReport(): Future[String] = {
    for {
      topAirports <- topCountriesByAirports()
      countryNames <- getCountryNames(topAirports.map(_._1).toSet)
    } yield {
      formatTopAirportsReport(topAirports, countryNames)
    }
  }
  
  def generateBottomAirportsReport(): Future[String] = {
    for {
      bottomAirports <- bottomCountriesByAirports()
      countryNames <- getCountryNames(bottomAirports.map(_._1).toSet)
    } yield {
      formatBottomAirportsReport(bottomAirports, countryNames)
    }
  }
  
  def generateRunwaySurfacesReport(): Future[String] = {
    for {
      surfaces <- runwaySurfacesByCountry()
      countryNames <- getCountryNames(surfaces.keys.toSet)
    } yield {
      formatRunwaySurfacesReport(surfaces, countryNames)
    }
  }
  
  def generateRunwayLatitudesReport(): Future[String] = {
    for {
      latitudes <- topRunwayLatitudes()
    } yield {
      formatRunwayLatitudesReport(latitudes)
    }
  }
  
  private def formatTopAirportsReport(data: Seq[(String, Int)], countryNames: Map[String, String]): String = {
    val header = "+--------------------------------------------------+\n" +
                 "|          TOP 10 COUNTRIES WITH MOST AIRPORTS      |\n" +
                 "+--------------------------------------------------+\n" +
                 "| Country                             | Airport Count |\n" +
                 "+--------------------------------------+--------------+"
                 
    val rows = data.map { case (code, count) =>
      val name = countryNames.getOrElse(code, code)
      val displayName = s"$code - $name"
      val truncatedName = if (displayName.length > 34) displayName.take(31) + "..." else displayName.padTo(34, ' ')
      s"| $truncatedName | ${count.toString.padTo(12, ' ')} |"
    }.mkString("\n")
    
    val footer = "+--------------------------------------+--------------+"
    
    s"$header\n$rows\n$footer"
  }
  
  private def formatBottomAirportsReport(data: Seq[(String, Int)], countryNames: Map[String, String]): String = {
    val header = "+--------------------------------------------------+\n" +
                 "|         COUNTRIES WITH LOWEST NUMBER OF AIRPORTS  |\n" +
                 "+--------------------------------------------------+\n" +
                 "| Country                             | Airport Count |\n" +
                 "+--------------------------------------+--------------+"
                 
    val rows = data.map { case (code, count) =>
      val name = countryNames.getOrElse(code, code)
      val displayName = s"$code - $name"
      val truncatedName = if (displayName.length > 34) displayName.take(31) + "..." else displayName.padTo(34, ' ')
      s"| $truncatedName | ${count.toString.padTo(12, ' ')} |"
    }.mkString("\n")
    
    val footer = "+--------------------------------------+--------------+"
    
    s"$header\n$rows\n$footer"
  }
  
  private def formatRunwayLatitudesReport(data: Seq[(String, Int)]): String = {
    val header = "+--------------------------------------------------+\n" +
                 "|           TOP 10 MOST COMMON RUNWAY LATITUDES     |\n" +
                 "+--------------------------------------------------+\n" +
                 "| Latitude Identifier                 | Count        |\n" +
                 "+--------------------------------------+--------------+"
                 
    val rows = data.map { case (ident, count) =>
      val truncatedIdent = if (ident.length > 34) ident.take(31) + "..." else ident.padTo(34, ' ')
      s"| $truncatedIdent | ${count.toString.padTo(12, ' ')} |"
    }.mkString("\n")
    
    val footer = "+--------------------------------------+--------------+"
    
    s"$header\n$rows\n$footer"
  }
  
  private def formatRunwaySurfacesReport(data: Map[String, Set[String]], countryNames: Map[String, String]): String = {
    val header = "+------------------------------------------------------------------+\n" +
                 "|                RUNWAY SURFACE TYPES BY COUNTRY                    |\n" +
                 "+------------------------------------------------------------------+"
    
    // Group countries by first letter for better organization
    val groupedByLetter = data.toSeq
      .sortBy(_._1)
      .groupBy(_._1.head)
      .toSeq
      .sortBy(_._1)
    
    val sections = groupedByLetter.map { case (letter, countries) =>
      val letterHeader = s"+--- $letter " + "-" * 60 + "+"
      
      val countryRows = countries.map { case (code, surfaces) =>
        val name = countryNames.getOrElse(code, code)
        val displayName = s"$code - $name"
        val truncatedName = if (displayName.length > 25) displayName.take(22) + "..." else displayName.padTo(25, ' ')
        
        // Format surfaces with line breaks if needed
        val formattedSurfaces = formatSurfaces(surfaces)
        s"| $truncatedName | $formattedSurfaces"
      }.mkString("\n")
      
      val letterFooter = "+" + "-" * 65 + "+"
      
      s"$letterHeader\n$countryRows\n$letterFooter"
    }.mkString("\n\n")
    
    s"$header\n\n$sections"
  }
  
  private def formatSurfaces(surfaces: Set[String]): String = {
    if (surfaces.isEmpty) return "No data available"
    
    // Limit to 5 surface types per line for readability
    val maxPerLine = 5
    val chunks = surfaces.toSeq.sorted.grouped(maxPerLine).toSeq
    
    if (chunks.size == 1) {
      chunks.head.mkString(", ")
    } else {
      chunks.head.mkString(", ") + ",\n" + 
      chunks.tail.map(chunk => "|" + " " * 28 + "| " + chunk.mkString(", ")).mkString(",\n")
    }
  }
  
  private def getCountryNames(codes: Set[String]): Future[Map[String, String]] = {
    dbService.getCountryNames(codes)
  }

  private def topCountriesByAirports(): Future[Seq[(String, Int)]] = {
    dbService.getTopCountriesByAirports()
  }
  
  private def bottomCountriesByAirports(): Future[Seq[(String, Int)]] = {
    dbService.getBottomCountriesByAirports()
  }

  private def runwaySurfacesByCountry(): Future[Map[String, Set[String]]] = {
    dbService.getRunwaySurfacesByCountry()
  }
  
  private def topRunwayLatitudes(): Future[Seq[(String, Int)]] = {
    dbService.getTopRunwayLatitudes()
  }
}