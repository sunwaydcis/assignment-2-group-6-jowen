import scala.io.Source
import scala.collection.mutable.ListBuffer

case class DataRow[T](
                       date: String,
                       state: String,
                       beds: T,
                       beds_covid: T,
                       beds_noncrit: T,
                       admitted_pui: T,
                       admitted_total: T,
                       discharged_covid: T,
                       discharged_total: T,
                       hosp_covid: T,
                       hosp_pui: T,
                       hosp_noncovid: T
                     )

object MyApp extends App {

  def readFile[T](convert: String => T): List[DataRow[T]] =
    val f = "src/main/resources/hospital.csv"
    val file = Source.fromFile(f)

    val lines = file.getLines().toList
    val headers = if lines.nonEmpty then lines.head.split(",").map(_.trim.stripPrefix("\uFEFF")).toList else List.empty


    def getIndex(headerName: String): Int =
      val index = headers.indexOf(headerName)
      if index == -1 then
        throw new NoSuchElementException(s"Header '$headerName' not found in the CSV file.")
      else
        index

    val hospitals = for line <- lines.tail yield
      val values = line.split(",").map(_.trim)

      DataRow(
        date = values(getIndex("date")),
        state = values(getIndex("state")),
        beds = convert(values(getIndex("beds"))),
        beds_covid = convert(values(getIndex("beds_covid"))),
        beds_noncrit = convert(values(getIndex("beds_noncrit"))),
        admitted_pui = convert(values(getIndex("admitted_pui"))),
        admitted_total = convert(values(getIndex("admitted_total"))),
        discharged_covid = convert(values(getIndex("discharged_covid"))),
        discharged_total = convert(values(getIndex("discharged_total"))),
        hosp_covid = convert(values(getIndex("hosp_covid"))),
        hosp_pui = convert(values(getIndex("hosp_pui"))),
        hosp_noncovid = convert(values(getIndex("hosp_noncovid")))
      )
    hospitals

  val hospitalData = readFile(_.toInt)


  def averageBeds(data: List[DataRow[Int]]): Map[String, (Double, Double, Double)] =
    data.groupBy(_.state).map:
      case (state, records) =>
        val suspectedAvg = records.map(_.admitted_pui).sum.toDouble / records.length
        val covidPositiveAvg = records.map(_.hosp_covid).sum.toDouble / records.length
        val nonCovidAvg = records.map(_.hosp_noncovid).sum.toDouble / records.length
        state -> (suspectedAvg, covidPositiveAvg, nonCovidAvg)

  val averages = averageBeds(hospitalData)

}
