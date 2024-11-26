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


}
