import scala.util.Try
import sttp.client4.quick.*
import sttp.client4.Response
import sttp.model.StatusCode

val SOLUTIONS: List[String => Unit] = List(
  solutions.Day01.solve,
  solutions.Day02.solve,
  solutions.Day03.solve,
  solutions.Day04.solve,
  solutions.Day05.solve,
  solutions.Day06.solve,
  solutions.Day07.solve,
  solutions.Day08.solve,
  solutions.Day09.solve,
  solutions.Day10.solve,
  solutions.Day11.solve,
  solutions.Day12.solve,
  solutions.Day13.solve,
  solutions.Day14.solve,
  solutions.Day15.solve,
  solutions.Day16.solve,
  solutions.Day17.solve,
  solutions.Day18.solve,
  solutions.Day19.solve,
  solutions.Day20.solve,
  solutions.Day21.solve,
  solutions.Day22.solve,
  solutions.Day23.solve,
  solutions.Day24.solve,
  solutions.Day25.solve,
)

@main def main(args: String*): Unit =
  if args.length != 1 then
    println("Day must be passed as a command line argument")
  else
    val parsed = Try(args(0).toInt).toEither
      .left.map(_ => "Day must be an integer")
      .flatMap(n => if n < 1 || n > 25 then Left("Day must be in range [1,25]") else Right(n))
    parsed match {
      case Right(day) => runDay(day)
      case Left(err) => println(s"Error occured: ${err}")
    }

def runDay(day: Int): Unit =
  val input = getInput(day)
  input match {
    case Right(s) => SOLUTIONS(day - 1)(s)
    case Left(err) => println(s"Couldn't get input for day ${day}: ${err}")
  }

def getInput(day: Int): Either[String, String] =
    val inputFile = os.pwd / "input" / f"day_${day}%02d.txt"
    if !os.exists(inputFile) then
      val year = sys.env.get("AOC_YEAR").getOrElse(java.time.LocalDate.now.getYear)
      val session = sys.env.get("AOC_SESSION").getOrElse("")
      val maybeResponse = Try(quickRequest
        .get(uri"https://adventofcode.com/${year}/day/${day}/input")
        .cookie("session", session)
        .send()).toOption
      if maybeResponse.isEmpty then
        return Left("Cound't send request to adventofcode.com")
      val response = maybeResponse.get
      response.code match {
        case StatusCode.Ok => os.write(inputFile, response.body, createFolders=true)
        case StatusCode.BadRequest => return Left("SESSION is not valid")
        case StatusCode.NotFound => return Left("Puzzle is not available yet")
      }
    Right(os.read(inputFile))