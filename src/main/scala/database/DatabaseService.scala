package database

import slick.jdbc.H2Profile.api._
import models.{Country, Airport, Runway}
import scala.concurrent.{ExecutionContext, Future}

class DatabaseService(implicit ec: ExecutionContext) {
  val db = Database.forConfig("h2mem")
  
  class Countries(tag: Tag) extends Table[Country](tag, "COUNTRIES") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
    def code = column[String]("CODE")
    def name = column[String]("NAME")
    def continent = column[String]("CONTINENT")
    def wikipediaLink = column[String]("WIKIPEDIA_LINK")
    def keywords = column[Option[String]]("KEYWORDS")
    
    def * = (id.?, code, name, continent, wikipediaLink, keywords) <> ((Country.apply _).tupled, Country.unapply)
  }
  
  class Airports(tag: Tag) extends Table[Airport](tag, "AIRPORTS") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
    def ident = column[String]("IDENT")
    def airportType = column[String]("TYPE")
    def name = column[String]("NAME")
    def latitude = column[Double]("LATITUDE")
    def longitude = column[Double]("LONGITUDE")
    def countryCode = column[String]("COUNTRY_CODE")
    
    def * = (id.?, ident, airportType, name, latitude, longitude, countryCode) <> ((Airport.apply _).tupled, Airport.unapply)
  }
  
  class Runways(tag: Tag) extends Table[Runway](tag, "RUNWAYS") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
    def airportRef = column[Int]("AIRPORT_REF")
    def airportIdent = column[String]("AIRPORT_IDENT")
    def lengthFt = column[Option[Int]]("LENGTH_FT")
    def widthFt = column[Option[Int]]("WIDTH_FT")
    def surface = column[Option[String]]("SURFACE")
    def lighted = column[Boolean]("LIGHTED")
    def closed = column[Boolean]("CLOSED")
    def leIdent = column[Option[String]]("LE_IDENT")
    def countryCode = column[String]("COUNTRY_CODE")
    
    def * = (id.?, airportRef, airportIdent, lengthFt, widthFt, surface, lighted, closed, leIdent, countryCode) <> ((Runway.apply _).tupled, Runway.unapply)
  }
  
  private val countries = TableQuery[Countries]
  private val airports = TableQuery[Airports]
  private val runways = TableQuery[Runways]
  
  def insertCountries(data: List[Country]): Future[Option[Int]] = 
    db.run(countries ++= data)
  
  def insertAirports(data: List[Airport]): Future[Option[Int]] = 
    db.run(airports ++= data)
  
  def insertRunways(data: List[Runway]): Future[Option[Int]] = 
    db.run(runways ++= data)
  
  def searchCountry(query: String): Future[Seq[Country]] = 
    db.run(
      countries.filter(c => 
        c.name.toLowerCase.like(s"%${query.toLowerCase}%") ||
        c.code.toLowerCase === query.toLowerCase
      ).result
    )

  def getTopCountriesByAirports(): Future[Seq[(String, Int)]] = 
    db.run(
      airports
        .groupBy(_.countryCode)
        .map { case (code, group) => (code, group.size) }
        .sortBy(_._2.desc)
        .take(10)
        .result
    )

  def getBottomCountriesByAirports(): Future[Seq[(String, Int)]] = 
    db.run(
      airports
        .groupBy(_.countryCode)
        .map { case (code, group) => (code, group.size) }
        .sortBy(_._2.asc)
        .take(10)
        .result
    )
    
  def getRunwaySurfacesByCountry(): Future[Map[String, Set[String]]] = {
    val query = for {
      runway <- runways if runway.surface.isDefined
    } yield (runway.countryCode, runway.surface.getOrElse(""))
    
    db.run(query.result).map { results =>
      results.groupBy(_._1).map { case (code, group) =>
        code -> group.map(_._2).toSet
      }
    }
  }

  def getTopRunwayLatitudes(): Future[Seq[(String, Int)]] = {
    val query = for {
      runway <- runways if runway.leIdent.isDefined
    } yield runway.leIdent.getOrElse("")
    
    db.run(query.result).map { results =>
      results
        .filterNot(_.isEmpty)
        .groupBy(identity)
        .map { case (ident, group) => (ident, group.size) }
        .toSeq
        .sortBy(-_._2)
        .take(10)
    }
  }

  def getCountryNames(codes: Set[String]): Future[Map[String, String]] = {
    db.run(
      countries.filter(c => c.code inSet codes)
        .map(c => (c.code, c.name))
        .result
    ).map(_.toMap)
  }

  def getAirportsAndRunwaysForCountry(countryCode: String): Future[(Seq[Airport], Seq[Runway])] = {
    val airportsQuery = airports.filter(_.countryCode === countryCode).result
    val runwaysQuery = runways.filter(_.countryCode === countryCode).result
    
    for {
      airportsResult <- db.run(airportsQuery)
      runwaysResult <- db.run(runwaysQuery)
    } yield (airportsResult, runwaysResult)
  }

  def createSchemas(): Future[Unit] = {
    db.run(DBIO.seq(
      countries.schema.createIfNotExists,
      airports.schema.createIfNotExists,
      runways.schema.createIfNotExists
    ))
  }
}
