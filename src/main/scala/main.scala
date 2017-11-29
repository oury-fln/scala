
import scala.io.Source
import Array._
import Math._

object main {

  /* 这是我的第一个 Scala 程序

   * 以下程序将输出'Hello World!'

   */

  def main(args: Array[String]) {
    knn(3, loadData("E:/work/code/scala/data.txt"), Array(1, 1))

  }

  /*
  load data from file
  parameter:
  path : String  file path
  return:
  Array result array
  file format:
  1 x -- count of date lines
  2 a b c -- x, y, label
  ...
   */
  def loadData(path: String): Array[Array[Int]] = {
    val file = Source.fromFile(path).getLines()
    val data = ofDim[Int](file.length, 3)
    for (i <- 0 until file.length) {
      val temp = file.next().split(" ")
      for (j <- 0 until temp.length) {
        data(i)(0) = temp(j)(0)
        data(i)(1) = temp(j)(1)
        data(i)(2) = temp(j)(2)
      }
    }
    //test for loaded result
    for (i <- 0 until file.length) {
      for (j <- 0 to 2) {
        println(data(i)(j))
      }
    }
    data
  }

  /*
  load data from file
  parameter:
  path : String  file path
  return:
  Array result array
  file format:
  1 x -- count of date lines
  2 a b c -- x, y, label
  ...
   */
  def knn(n: Int, data: Array[Array[Int]], newPoint: Array[Int]): Unit = {
    if (n > data.length) {
      println("n is bigger than the number of data, using the number of data as n.")
      val n = data.length
    }
    compute(data, newPoint)
  }
  def compute(data: Array[Array[Int]], newPoint: Array[Int]): Unit = {
    val distance = new Array[Double](data.length)
    for (i <- 0 to data.length - 1) {
      distance(i) = sqrt(pow(data(i)(0) - newPoint(0), 2) + pow(data(i)(1) - newPoint(1), 2))
    }
    distance.foreach{
      println
    }
  }
}