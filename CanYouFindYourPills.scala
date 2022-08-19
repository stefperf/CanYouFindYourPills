import scala.collection.mutable.{ListBuffer, Map => MutableMap}
import scala.util.Random

object CanYouFindYourPills extends App {

  val fullPillDose: Double = 1.0
  val halfPillDose: Double = 0.5
  val dailyDose: Double = fullPillDose + halfPillDose

  abstract class Pill { def dose: Double }
  case object FullPill extends Pill { def dose: Double = fullPillDose }
  case object HalfPill extends Pill { def dose: Double = halfPillDose }

  class PillBottle(nrDays: Int) {
    private val _pills: ListBuffer[Pill] = ListBuffer.empty
    val nrInitialPills: Double = dailyDose * nrDays
    val nrInitialFullPills = nrInitialPills.floor.toInt
    (1 to nrInitialFullPills).foreach { _ => insertPill(FullPill) }
    val nrInitialHalfPills =
      if (nrInitialFullPills < nrInitialPills) { insertPill(HalfPill); 1 }
      else 0

    def takesFullDosageOnLastDay(): Boolean = {
      (1 to (nrDays - 1)).foreach { _ => takeDailyDose() }
      takeTwoPillsOnLastDay()
    }

    private def takeDailyDose(): Unit = {
      val twoPills: Seq[Pill] = (1 to 2).map { _ => extractPill() }
      val dose = twoPills.map(_.dose).sum
      if (dose > dailyDose)
        insertPill(HalfPill)
      else if (dose < dailyDose) {
        val thirdPill = extractPill()
        if (thirdPill.dose == fullPillDose)
          insertPill(HalfPill)
      }
    }

    private def takeTwoPillsOnLastDay(): Boolean = {
      val twoPills: Seq[Pill] = (1 to 2).map { _ => extractPill() }
      val dose = twoPills.map(_.dose).sum
      dose == dailyDose
    }

    def extractPill(): Pill = _pills.remove(Random.nextInt(_pills.size))

    def insertPill(pill: Pill): Unit = _pills.append(pill)
  }

  def simulate(nrDays: Int, nrScenarios: Int = 1000000): Unit = {
    var nrSuccesses = 0
    (1 to nrScenarios).foreach { _ =>
      val success: Boolean = new PillBottle(nrDays).takesFullDosageOnLastDay()
      if (success) nrSuccesses += 1
    }
    println(
      s"With $nrDays days, the right dose was achieved in " +
        s"$nrSuccesses / $nrScenarios ~= ${nrSuccesses.toDouble / nrScenarios} simulated scenarios."
    )
  }

  case class Fraction(n_ : Long, d_ : Long) {
    private val myGcd =
      if (n_ == 0L) 1L
      else if (n_ >= d_) gcd(n_, d_)
      else gcd(d_, n_)
    private val _n = n_ / myGcd
    def n: Long = _n
    private val _d = d_ / myGcd
    def d: Long = _d
    def +(that: Fraction): Fraction =
      Fraction(n * that.d + that.n * d, d * that.d)
    def -(that: Fraction): Fraction =
      Fraction(n * that.d - that.n * d, d * that.d)
    def *(that: Fraction): Fraction = Fraction(n * that.n, d * that.d)
    def /(that: Fraction): Fraction = Fraction(n * that.d, d * that.n)
    override def toString = s"$n / $d ~= ${n.toDouble / d}"

    // expects larger >= smaller
    private def gcd(larger: Long, smaller: Long): Long = {
      val remainder = larger % smaller
      if (remainder == 0L) smaller
      else gcd(smaller, remainder)
    }

  }

  val alreadyCalculatedProbabilities = MutableMap.empty[(Int, Int), Fraction]

  def successProbability(nrFullPills: Int, nrHalfPills: Int): Fraction =
    alreadyCalculatedProbabilities.getOrElse(
      (nrFullPills, nrHalfPills), {
        val thisSuccessProbability = {
          val itsTheLastDay = nrFullPills * 1.0 + nrHalfPills * 0.5 == 1.5
          if (itsTheLastDay) // base case
            if (nrHalfPills == 3) Fraction(0, 1)
            else Fraction(1, 1)
          else { // recursion
            if (nrFullPills == 0) Fraction(0, 1)
            else if (nrHalfPills == 0)
              successProbability(nrFullPills - 2, nrHalfPills + 1)
            else {
              val nrAnyPills = nrFullPills + nrHalfPills
              val probabilityOfExtractingTwoFullPills =
                Fraction(
                  nrFullPills * (nrFullPills - 1),
                  nrAnyPills * (nrAnyPills - 1)
                )
              val contributionOfExtractingTwoFullPills =
                probabilityOfExtractingTwoFullPills * successProbability(
                  nrFullPills - 2,
                  nrHalfPills + 1
                )
              val (
                probabilityOfExtractingThreeHalfPills,
                contributionOfExtractingThreeHalfPills
              ) =
                if (nrHalfPills < 3) (Fraction(0, 1), Fraction(0, 1))
                else {
                  val probabilityOfExtractingThreeHalfPills =
                    Fraction(
                      nrHalfPills * (nrHalfPills - 1) * (nrHalfPills - 2),
                      nrAnyPills * (nrAnyPills - 1) * (nrAnyPills - 2)
                    )
                  val contributionOfExtractingThreeHalfPills =
                    probabilityOfExtractingThreeHalfPills * successProbability(
                      nrFullPills,
                      nrHalfPills - 3
                    )
                  (
                    probabilityOfExtractingThreeHalfPills,
                    contributionOfExtractingThreeHalfPills
                  )
                }
              val residualProbability =
                Fraction(
                  1,
                  1
                ) - probabilityOfExtractingTwoFullPills - probabilityOfExtractingThreeHalfPills
              val residualContribution =
                residualProbability * successProbability(
                  nrFullPills - 1,
                  nrHalfPills - 1
                )
              contributionOfExtractingTwoFullPills + contributionOfExtractingThreeHalfPills + residualContribution
            }
          }
        }
        alreadyCalculatedProbabilities((nrFullPills, nrHalfPills)) =
          thisSuccessProbability
        thisSuccessProbability
      }
    )

  (1 to 15).foreach { nrDays =>
    simulate(nrDays)
    val theoreticalSuccessProbability = {
      val pillBottle = new PillBottle(nrDays)
      successProbability(
        pillBottle.nrInitialFullPills,
        pillBottle.nrInitialHalfPills
      )
    }
    println(
      s"With $nrDays days, the exact success probability is $theoreticalSuccessProbability."
    )
  }

}
