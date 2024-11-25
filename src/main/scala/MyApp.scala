import scala.io.Source
import scala.collection.mutable.ListBuffer

case class DataRow(
                    date: String,
                    state: String,
                    beds: Int,
                    beds_covid: Int,
                    beds_noncrit: Int,
                    admitted_pui: Int,
                    admitted_total: Int,
                    discharged_covid: Int,
                    discharged_total: Int,
                    hosp_covid: Int,
                    hosp_pui: Int,
                    hosp_noncovid: Int
                  )

object MyApp extends App {

  def readFile(): List[DataRow] =
    val f = "src/main/resources/hospital.csv"
    val file = Source.fromFile(f)

    val lines = file.getLines().toList
    val headers = if lines.nonEmpty then lines.head.split(",").map(_.trim).toList else List.empty

    val hospitals = for line <- lines.tail yield
      val values = line.split(",").map(_.trim)
      DataRow(
        date = values(headers.indexOf("date")),
        state = values(headers.indexOf("state")),
        beds = values(headers.indexOf("beds")).toInt,
        beds_covid = values(headers.indexOf("beds_covid")).toInt,
        beds_noncrit = values(headers.indexOf("beds_noncrit")).toInt,
        admitted_pui = values(headers.indexOf("admitted_pui")).toInt,
        admitted_total = values(headers.indexOf("admitted_total")).toInt,
        discharged_covid = values(headers.indexOf("discharged_covid")).toInt,
        discharged_total = values(headers.indexOf("discharged_total")).toInt,
        hosp_covid = values(headers.indexOf("hosp_covid")).toInt,
        hosp_pui = values(headers.indexOf("hosp_pui")).toInt,
        hosp_noncovid = values(headers.indexOf("hosp_noncovid")).toInt
      )
    hospitals

  val hospitalData = readFile()

  // Print some rows to verify
  //hospitalData.foreach(println)

  def extractYear(dataRow: DataRow): String = dataRow.date.split("/")(2)

  def stateWithHighestBedsPerYear(data: List[DataRow]): Map[String, (String, Int)] =
    val highestBedsPerYear = data.groupBy(extractYear).map { case (year, entries) =>
      val stateWithMaxBeds = entries.groupBy(_.state).view.mapValues(_.maxBy(_.beds)).values.maxBy(_.beds)
      year -> (stateWithMaxBeds.state, stateWithMaxBeds.beds)
    }

    highestBedsPerYear

  val highestBedsPerYear = stateWithHighestBedsPerYear(hospitalData)
  highestBedsPerYear.foreach { case (year, (state, beds)) =>
    println(s"In $year, the state with the highest total hospital beds is $state with $beds beds.")
  }

  def covidBedRatio(data:List[DataRow]) : Double =
    val totalBeds = data.map(_.beds).sum
    val totalCovidBeds = data.map(_.beds_covid).sum
    totalCovidBeds.toDouble / totalBeds.toDouble

  val ratio = covidBedRatio(hospitalData)
  println(f"The ratio of beds dedicated for COVID-19 to total available hospital beds is $ratio%.2f")

  def categoryAverages(data: List[DataRow]): Map[String, (Double, Double, Double)] =
    data.groupBy(_.state).map:
      case (state, records) =>
        val suspectedAvg = records.map(_.admitted_pui).sum.toDouble / records.length
        val covidPositiveAvg = records.map(_.hosp_covid).sum.toDouble / records.length
        val nonCovidAvg = records.map(_.hosp_noncovid).sum.toDouble / records.length
        state -> (suspectedAvg, covidPositiveAvg, nonCovidAvg)

  // Display the answer
  val averages = categoryAverages(hospitalData)
  averages.foreach:
    case (state, (suspectedAvg, covidPositiveAvg, nonCovidAvg)) =>
      println(f"In $state: Suspected average = $suspectedAvg%.2f, COVID-19 positive average = $covidPositiveAvg%.2f, Non-COVID average = $nonCovidAvg%.2f")

}
// IM SORRY I NEVER USED GITHUB