import java.io.File
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import database.DatabaseService
import processing.DataLoader
import reports.ReportService
import ui.CliInterface

object Main extends App {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val dbService = new DatabaseService()
  val dataLoader = new DataLoader()
  val reportService = new ReportService(dbService)
  val cliInterface = new CliInterface(dbService, reportService)

  try {
    Await.result(dbService.createSchemas(), 1.minute)
    
    val countriesFile = new File("src/main/resources/countries.csv")
    val airportsFile = new File("src/main/resources/airports.csv")
    val runwaysFile = new File("src/main/resources/runways.csv")
    
    if (!countriesFile.exists()) {
      println(s"Countries file not found at: ${countriesFile.getAbsolutePath}")
      System.exit(1)
    }
    if (!airportsFile.exists()) {
      println(s"Airports file not found at: ${airportsFile.getAbsolutePath}")
      System.exit(1)
    }
    if (!runwaysFile.exists()) {
      println(s"Runways file not found at: ${runwaysFile.getAbsolutePath}")
      System.exit(1)
    }
    
    val result = Await.result(
      dataLoader.loadDataParallel(
        countriesFile,
        airportsFile,
        runwaysFile
      ),
      1.minute
    )
    
    result match {
      case Right((countries, airports, runways)) =>
        println(s"Loaded ${countries.size} countries, ${airports.size} airports, ${runways.size} runways")
        
        val airportToCountry = airports.map(a => a.ident -> a.countryCode).toMap
        
        val runwaysWithCountry = runways.map(r => 
          r.copy(countryCode = airportToCountry.getOrElse(r.airportIdent, ""))
        )
        
        Await.result(
          Future.sequence(Seq(
            dbService.insertCountries(countries),
            dbService.insertAirports(airports),
            dbService.insertRunways(runwaysWithCountry)
          )),
          1.minute
        )
        
        cliInterface.start()
        
      case Left(error) =>
        println(s"Error loading data: $error")
        System.exit(1)
    }
  } catch {
    case e: Exception =>
      println(s"Fatal error: ${e.getMessage}")
      e.printStackTrace()
      System.exit(1)
  }
}