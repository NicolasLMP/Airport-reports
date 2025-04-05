package models

case class Runway(
  id: Option[Int] = None,
  airportRef: Int,
  airportIdent: String,
  lengthFt: Option[Int],
  widthFt: Option[Int],
  surface: Option[String],
  lighted: Boolean,
  closed: Boolean,
  leIdent: Option[String], // Added for runway latitude report
  countryCode: String // This is not in the CSV but needed for our queries
)

object Runway {
  def from(csvLine: String): Either[String, Runway] = {
    try {
      val fields = csvLine.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1)
      if (fields.length < 9) Left(s"Invalid number of fields in line: $csvLine")
      else Right(Runway(
        id = Some(fields(0).replaceAll("\"", "").trim.toInt),
        airportRef = fields(1).replaceAll("\"", "").trim.toInt,
        airportIdent = fields(2).replaceAll("\"", "").trim,
        lengthFt = Option(fields(3)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim.toInt),
        widthFt = Option(fields(4)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim.toInt),
        surface = Option(fields(5)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim),
        lighted = fields(6).replaceAll("\"", "").trim == "1",
        closed = fields(7).replaceAll("\"", "").trim == "1",
        leIdent = Option(fields(8)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim),
        countryCode = "" // Will be populated later based on airport data
      ))
    } catch {
      case e: Exception => Left(s"Error parsing line: ${e.getMessage} in line: $csvLine")
    }
  }
}
