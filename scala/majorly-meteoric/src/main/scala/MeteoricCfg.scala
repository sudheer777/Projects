import ErrorHandling.ErrorRecord
import org.json4s.jackson.JsonMethods.parse
import org.json4s.jackson.Serialization.write
import org.json4s.{DefaultFormats, Formats}

object MeteoricCfg {
  implicit val formats: Formats = DefaultFormats
  case class GeoLocation(latitude: Option[String], longitude: Option[String])

  case class MeteoricRecord(name: Option[String], id: Option[String], nametype: Option[String], recclass: Option[String], mass: Option[String], fall: Option[String],
                            year: Option[String], reclat: Option[String], reclong: Option[String], geolocation: GeoLocation) {

    private var isValidMass = true
    val massDouble: Double = try {
      mass.get.toDouble
    } catch {
      case _: Exception =>
        isValidMass = false
        0D
    }

    def isValid(fileName: String): Either[MeteoricRecord, ErrorRecord] = {
      if (id.isEmpty) {
        Right(ErrorRecord("RECORD ERROR", fileName, "id column is not defined", write(this)))
      } else if (year.isEmpty) {
        Right(ErrorRecord("RECORD ERROR", fileName, "year column is not defined", write(this)))
      } else if (!isValidMass) {
        if (mass.isEmpty) Right(ErrorRecord("RECORD ERROR", fileName, "mass column is not defined",  write(this)))
        else Right(ErrorRecord("RECORD ERROR", fileName, "mass column value is invalid",  write(this)))
      } else {
        Left(this)
      }
    }
  }

  def load(fileName: String, json: String): Seq[Either[MeteoricRecord, ErrorRecord]] = {
    try {
      val js = parse(json)
      val recs = js.extract[Array[MeteoricRecord]]
      recs.map(_.isValid(fileName))
    } catch {
      case e: Exception => Seq(Right(ErrorRecord("FILE ERROR", fileName, e.toString, "NULL")))
    }
  }
}