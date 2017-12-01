
import scala.io.Source
import Math._
import scala.collection.mutable.Map

object main {

  /* first and maybe last scala program
   * if will work out knn in the FP way as far as possible
   * I tried my best
   */

  /*
   * main function
   * parameter:
   *   args :
   *     0 -- path of data file
   *     1 -- the aim point x(double)
   *     2 -- the aim point y(double)
   *     3 -- k in 'k'-nn
   * ps. file format: a b c -- x, y, label
   */
  def main(args: Array[String]) {
    knn(args(3).toInt, args(0), Array(args(1).toDouble, args(2).toDouble))
  }

  /*
   * knn main part, statistic the
   * parameter:
   *   k : 'k'-nn
   *   path : path of data file
   *   newPoint : the aim point x, y
   * return:
   *   whatever
   */
  def knn(k: Int, path: String, newPoint: Array[Double]): Unit = {
    findFirstK(k, compute(path, newPoint)).foreach(i => println("label: " + i._1 + " times: " + i._2))
  }

  /*
   * knn distance compute part
   * parameter:
   *   path : path of data file
   *   newPoint : the aim point x, y
   * return:
   *   the list of distance with label
   */
  def compute(path: String, newPoint: Array[Double]): List[(Double, Int)] = {
    var distances: List[(Double, Int)] = List()
    Source.fromFile(path).getLines().foreach ( line => distances = distances :+ toDistance(line, newPoint))
    distances
  }

  /*
   * main distance compute part
   * parameter:
   *   line : a line of data like : x(double) y(double) label(int)
   *   newPoint : the aim point x, y
   * return:
   *   the distance between the line of data with aim point
   */
  def toDistance(line: String, newPoint: Array[Double]): (Double, Int) = {
    val pieces = line.split(" ")
    (euclideanDistance(Array(pieces(0).toDouble, pieces(1).toDouble), newPoint), pieces(2).toInt)
  }

  /*
   * euclidean distance compute
   * parameter:
   *   point1 : first point
   *   point2 : second point
   * return:
   *   the distance between the two points
   */
  def euclideanDistance(point1: Array[Double], point2: Array[Double]): Double = {
    sqrt(pow(point1(0) - point2(0), 2) + pow(point1(1) - point2(1), 2))
  }

  /*
   * statistic the first k label
   * parameter:
   *   k : 'k'-nn
   *   list : the list of distance with label
   * return:
   *   Map of label and times
   */
  def findFirstK(k: Int, list: List[(Double, Int)]): Map[Int, Int] = {
    val rank: Map[Int, Int] = Map.empty
    val data = list.sorted.toArray
    for (i <- 0 until k if data.length > i) {
      val label = data(i)._2
      rank(label) = rank.getOrElse(label, 0) + 1
    }
    rank
  }
}