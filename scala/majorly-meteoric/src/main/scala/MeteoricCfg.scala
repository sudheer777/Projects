import ErrorHandling.ErrorRecord
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{DefaultFormats, Formats}

object MeteoricCfg {
  case class GeoLocation(latitude: Option[String], longitude: Option[String]) {
    def isValid: Boolean = {
      true
    }
  }

  case class MeteoricRecord(name: Option[String], id: Option[String], nametype: Option[String], recclass: Option[String], mass: Option[Double], fall: Option[String],
                            year: Option[String], reclat: Option[String], reclong: Option[String], geolocation: GeoLocation) {
    def isValid(fileName: String): Either[MeteoricRecord, ErrorRecord] = {
      if (id.isEmpty) {
        Right(ErrorRecord("RECORD ERROR", fileName, "id column is not defined"))
      } else if (year.isEmpty) {
        Right(ErrorRecord("RECORD ERROR", fileName, "year column is not defined"))
      } else if (mass.isEmpty) {
        Right(ErrorRecord("RECORD ERROR", fileName, "mass column is not defined"))
      } else {
        Left(this)
      }
    }
  }

  def load(fileName: String, json: String): Seq[Either[MeteoricRecord, ErrorRecord]] = {
    implicit val formats: Formats = DefaultFormats
    try {
      val js = parse(json)
      val recs = js.extract[Array[MeteoricRecord]]
      recs.map(_.isValid(fileName))
    } catch {
      case e: Exception => Seq(Right(ErrorRecord("FILE ERROR", fileName, e.toString)))
    }
  }
}