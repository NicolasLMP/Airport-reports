package models

case class Country(
  id: Option[Int] = None,
  code: String,
  name: String,
  continent: String,
  wikipediaLink: String,
  keywords: Option[String]
)

object Country {
  def from(csvLine: String): Either[String, Country] = {
    try {
      val fields = csvLine.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1)
      if (fields.length < 6) Left(s"Invalid number of fields in line: $csvLine")
      else Right(Country(
        id = Option(fields(0)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim.toInt),
        code = fields(1).replaceAll("\"", "").trim,
        name = fields(2).replaceAll("\"", "").trim,
        continent = fields(3).replaceAll("\"", "").trim,
        wikipediaLink = fields(4).replaceAll("\"", "").trim,
        keywords = Option(fields(5)).filter(_.nonEmpty).map(_.replaceAll("\"", "").trim)
      ))
    } catch {
      case e: Exception => Left(s"Error parsing line: ${e.getMessage}")
    }
  }
}

