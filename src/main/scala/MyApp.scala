import scala.io.Source
import scala.util.{Try, Using}

// Base class for processing hospital data
abstract class HospitalDataProcessor[T] {
  def convert(value: String): T
  def processData(filePath: String): List[DataRow[T]]
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


