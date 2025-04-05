package ui

import database.DatabaseService
import reports.ReportService
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import scala.io.StdIn
import models.{Country, Airport, Runway}

class CliInterface(dbService: DatabaseService, reportService: ReportService)(implicit ec: ExecutionContext) {
  def start(): Unit = {
    println("\nWelcome to Airport Reports!")
    var running = true
    
    while (running) {
      println("\nChoose an option:")
      println("1. Query option")
      println("2. View airport reports")
      println("3. Exit")
      
      StdIn.readLine() match {
        case "1" => queryMode()
        case "2" => reportMode()
        case "3" => running = false
        case _ => println("Invalid option")
      }
    }
  }

  private def queryMode(): Unit = {
    println("\n=== Query Option ===")
    println("This option allows you to search for airports and runways by country name or code.")
    print("Enter country name or code: ")
    val input = StdIn.readLine()
    
    try {
      val countries = Await.result(dbService.searchCountry(input), 5.seconds)
      if (countries.isEmpty) {
        println("No countries found matching your query.")
      } else if (countries.size > 1) {
        println("\nMultiple countries found. Please select one:")
        countries.zipWithIndex.foreach { case (country, index) =>
          println(s"${index + 1}. ${country.name} (${country.code})")
        }
        
        print("Enter selection number: ")
        val selection = StdIn.readLine()
        try {
          val index = selection.toInt - 1
          if (index >= 0 && index < countries.size) {
            displayCountryDetails(countries(index))
          } else {
            println("Invalid selection.")
          }
        } catch {
          case _: NumberFormatException => println("Invalid input. Please enter a number.")
        }
      } else {
        // Only one country found, display its details directly
        displayCountryDetails(countries.head)
      }
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
  
  private def displayCountryDetails(country: Country): Unit = {
    println(s"\n=== Country Information ===")
    println(s"Country: ${country.name}")
    println(s"Code: ${country.code}")
    
    try {
      val (airports, runways) = Await.result(
        dbService.getAirportsAndRunwaysForCountry(country.code), 
        5.seconds
      )
      
      if (airports.isEmpty) {
        println("\nNo airports found for this country.")
      } else {
        println(s"\n=== Airports in ${country.name} (${airports.size} total) ===")
        airports.foreach { airport =>
          println(s"\n${airport.name} (${airport.ident})")
          println(s"  Type: ${airport.airportType}")
          
          // Find runways for this airport
          val airportRunways = runways.filter(_.airportIdent == airport.ident)
          if (airportRunways.nonEmpty) {
            println(s"  Runways (${airportRunways.size}):")
            airportRunways.foreach { runway =>
              val surface = runway.surface.getOrElse("Unknown")
              val length = runway.lengthFt.map(l => s"${l}ft").getOrElse("Unknown length")
              val width = runway.widthFt.map(w => s"${w}ft").getOrElse("Unknown width")
              println(s"    - Surface: $surface, Size: $length x $width, Lighted: ${runway.lighted}, Closed: ${runway.closed}")
            }
          } else {
            println("  No runway information available")
          }
        }
      }
    } catch {
      case e: Exception => println(s"Error retrieving airport data: ${e.getMessage}")
    }
  }

  private def reportMode(): Unit = {
    var reportMenuRunning = true
    
    while (reportMenuRunning) {
      println("\nSelect a report to view:")
      println("1. Top 10 countries with most airports")
      println("2. Countries with lowest number of airports")
      println("3. Runway surface types by country")
      println("4. Top 10 most common runway latitudes")
      println("5. View all reports")
      println("6. Return to main menu")
      
      StdIn.readLine() match {
        case "1" => showTopAirportsReport()
        case "2" => showBottomAirportsReport()
        case "3" => showRunwaySurfacesReport()
        case "4" => showRunwayLatitudesReport()
        case "5" => showAllReports()
        case "6" => reportMenuRunning = false
        case _ => println("Invalid option")
      }
    }
  }
  
  private def showTopAirportsReport(): Unit = {
    try {
      val report = Await.result(reportService.generateTopAirportsReport(), 5.seconds)
      println("\nTop 10 Countries with Most Airports:")
      println(report)
    } catch {
      case e: Exception => println(s"Error generating report: ${e.getMessage}")
    }
  }
  
  private def showRunwaySurfacesReport(): Unit = {
    try {
      val report = Await.result(reportService.generateRunwaySurfacesReport(), 5.seconds)
      println("\nRunway Surface Types by Country:")
      println(report)
    } catch {
      case e: Exception => println(s"Error generating report: ${e.getMessage}")
    }
  }
  
  private def showAllReports(): Unit = {
    try {
      val reports = Await.result(reportService.generateReports(), 5.seconds)
      println("\nAirport Reports:")
      reports.foreach(println)
    } catch {
      case e: Exception => println(s"Error generating reports: ${e.getMessage}")
    }
  }
  
  private def showBottomAirportsReport(): Unit = {
    try {
      val report = Await.result(reportService.generateBottomAirportsReport(), 5.seconds)
      println("\nCountries with Lowest Number of Airports:")
      println(report)
    } catch {
      case e: Exception => println(s"Error generating report: ${e.getMessage}")
    }
  }
  
  private def showRunwayLatitudesReport(): Unit = {
    try {
      val report = Await.result(reportService.generateRunwayLatitudesReport(), 5.seconds)
      println("\nTop 10 Most Common Runway Latitudes:")
      println(report)
    } catch {
      case e: Exception => println(s"Error generating report: ${e.getMessage}")
    }
  }
}