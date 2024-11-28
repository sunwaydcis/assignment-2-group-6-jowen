import scala.io.Source
import scala.util.{Try, Using}

// Base class for processing hospital data
abstract class HospitalDataProcessor[T] {
  def convert(value: String): T
  def processData(filePath: String): List[DataRow[T]]
}

// Concrete class for processing integer-based hospital data
class IntegerHospitalDataProcessor extends HospitalDataProcessor[Int] {
  def convert(value: String): Int = value.toInt

  def processData(filePath: String): List[DataRow[Int]] =
    Using(Source.fromFile(filePath)) { file =>
      val lines = file.getLines().toList
      val headers = if lines.nonEmpty then lines.head.split(",").map(_.trim).toList else List.empty

      def getIndex(headerName: String): Int =
        headers.indexOf(headerName) match
          case -1 => throw new NoSuchElementException(s"Header '$headerName' not found.")
          case index => index

      val hospitals = for line <- lines.tail yield
        val values = line.split(",").map(_.trim)
        DataRow(
          date = values(getIndex("date")),
          state = values(getIndex("state")),
          beds = convert(values(getIndex("beds"))),
          beds_covid = convert(values(getIndex("beds_covid"))),
          beds_noncrit = convert(values(getIndex("beds_noncrit"))),
          admitted_pui = convert(values(getIndex("admitted_pui"))),
          admitted_covid = convert(values(getIndex("admitted_covid"))),
          admitted_total = convert(values(getIndex("admitted_total"))),
          discharged_covid = convert(values(getIndex("discharged_covid"))),
          discharged_total = convert(values(getIndex("discharged_total"))),
          hosp_covid = convert(values(getIndex("hosp_covid"))),
          hosp_pui = convert(values(getIndex("hosp_pui"))),
          hosp_noncovid = convert(values(getIndex("hosp_noncovid")))
        )
      hospitals
    }.getOrElse {
      println("Error reading file.")
      List.empty
    }
}

// Case class representing a data row
case class DataRow[T](
                       date: String,
                       state: String,
                       beds: T,
                       beds_covid: T,
                       beds_noncrit: T,
                       admitted_pui: T,
                       admitted_covid: T,
                       admitted_total: T,
                       discharged_covid: T,
                       discharged_total: T,
                       hosp_covid: T,
                       hosp_pui: T,
                       hosp_noncovid: T
                     )

object MyApp extends App {

  val processor = IntegerHospitalDataProcessor()
  val hospitalData = processor.processData("src/main/resources/hospital.csv")

  // 1. State with the highest total hospital beds
  def stateWithHighestBeds(data: List[DataRow[Int]]): (String, Int) =
    data.groupBy(_.state).view.mapValues(_.map(_.beds).sum).toSeq.maxBy(_._2)

  val (state, highestBeds) = stateWithHighestBeds(hospitalData)
  println("Question 1")
  println(s"The state with the highest total hospital beds is $state with $highestBeds beds.")


  // 3. Average suspected and covid beds
  def averageAdmissions(data: List[DataRow[Int]]): Map[String, (Double, Double)] =
    data.groupBy(_.state).view.mapValues { records =>
      val totalSuspected = records.map(_.admitted_pui).sum
      val totalCovid = records.map(_.admitted_covid).sum
      val avgSuspected = totalSuspected.toDouble / records.size
      val avgCovid = totalCovid.toDouble / records.size
      (avgSuspected, avgCovid)
    }.toMap

  val averages = averageAdmissions(hospitalData)

  println("\nQuestion 3")
  averages.foreach { case (state, (avgSuspected, avgCovid)) =>
    println(f"In $state: Suspected average = $avgSuspected%.2f,  COVID-19 average = $avgCovid%.2f")
  }
}
