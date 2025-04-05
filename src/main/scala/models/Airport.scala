package models

case class Airport(
  id: Option[Int] = None,
  ident: String,
  airportType: String,
  name: String,
  latitude: Double,
  longitude: Double,
  countryCode: String
)

object Airport {
  def from(csvLine: String): Either[String, Airport] = {
    try {
      val fields = csvLine.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1)
      if (fields.length < 9) Left(s"Invalid number of fields in line: $csvLine")
      else Right(Airport(
        id = Some(fields(0).replaceAll("\"", "").trim.toInt),
        ident = fields(1).replaceAll("\"", "").trim,
        airportType = fields(2).replaceAll("\"", "").trim,
        name = fields(3).replaceAll("\"", "").trim,
        latitude = fields(4).replaceAll("\"", "").trim.toDouble,
        longitude = fields(5).replaceAll("\"", "").trim.toDouble,
        countryCode = fields(8).replaceAll("\"", "").trim
      ))
    } catch {
      case e: Exception => Left(s"Error parsing line: ${e.getMessage} in line: $csvLine")
    }
  }
}
