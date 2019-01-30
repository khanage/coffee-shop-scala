import cats._
import cats.effect.IO
import cats.implicits._

object Main {
  def main(args: Array[String]): Unit = {
    implicit val customerInteraction: CustomerInteraction[IO] = ???
    implicit val paymentGateway: PaymentGateway[IO] = ???
    implicit val coffeeMachine: CoffeeMachine[IO] = ???

    // here we can pick between different implementations
    // of our price lookup
// 1. We can just use a hardcoded set of values, but it won't impact the program
   implicit val hardCodedPricingLookup = PricingLookup.fromPriceList[IO](1.0, _ => 1.0, _ => 0.0)
// 2. We can select a different instance that will lookup the price of some items in a "database"
//    implicit val milkDb: PricingLookup.MilkDb[IO] = new PricingLookup.MilkDb[IO] {
//      override def priceForSoy: IO[Double] = IO { 2.0 }
//    }
//    implicit val withDbLookup = PricingLookup.fromPriceListAndDbSomethings[IO](1.0, _ => 1.0, _ => 0.0)

    // This is where we turn things into the real world from a program that could otherwise be
    // tested with mock implementations etc
    waitForCustomer[IO].unsafeRunSync()
  }

  def waitForCustomer[F[_]: Monad : PricingLookup]
  (implicit
   paymentGateway: PaymentGateway[F],
   coffeeMachine: CoffeeMachine[F],
   customerInteraction: CustomerInteraction[F])
  : F[Unit] = for {
    maybeOrder <- getCustomersOrder[F]

    _ <- maybeOrder match {
      case Some((coffee: Coffee, charge: Charge)) => for {
        _ <- paymentGateway.singlePayment(charge)
        brewedCoffee <- coffeeMachine.makeCoffee(coffee)
        _ <- customerInteraction.handOverCoffee(brewedCoffee)
      } yield ()
      case None => waitForCustomer[F]
    }
  } yield ()

  def getCustomersOrder[F[_] : Monad]
  (implicit customerInteraction: CustomerInteraction[F], pricingLookup: PricingLookup[F])
  : F[Option[(Coffee, Charge)]] = for {
    coffee <- customerInteraction.getCoffeeForOrder
    charge <- pricingLookup.costForCoffee(coffee)
    shouldContinue <- customerInteraction.confirmCostFor(coffee, charge)
  } yield {
    if (shouldContinue) Some((coffee, charge)) else Option.empty
  }
}

case class Coffee(shots: Int, milk: Option[Milk], sugar: Option[Int])
case class Charge(amount: Double)

case class Card(cardNumber: String, expiryMonth: Int, expiryYear: Int, ccv: Int)
case class Receipt(value: Double)

sealed trait Milk
case object Soy extends Milk
case object FullCream extends Milk
case object Skim extends Milk

trait PricingLookup[F[_]] {
  def costForCoffee(coffee: Coffee): F[Charge]
}

object PricingLookup {
  def fromPriceList[F[_]: Applicative]
  (costPerShot: Double, costForMilk: Milk => Double, costForSugar: Int => Double)
  : PricingLookup[F] = (coffee: Coffee) => Applicative[F].pure {
    val shotCost = costPerShot * coffee.shots
    val milkCost = coffee.milk.map(costForMilk).getOrElse(0.0)
    val sugarCost = coffee.sugar.map(costForSugar).getOrElse(0.0)
    Charge(shotCost + milkCost + sugarCost)
  }

  def fromPriceListAndDbSomethings[F[_]: Monad]
  (costPerShot: Double, costForMilk: Milk => Double, costForSugar: Int => Double)
  (implicit milkDb: MilkDb[F])
  : PricingLookup[F] = (coffee: Coffee) => for {
    milkCost <- coffee.milk match {
      case Some(Soy) => milkDb.priceForSoy
      case Some(anythingElse) => Monad[F].pure(costForMilk(anythingElse))
      case None => Monad[F].pure(0.0)
    }
  } yield {
    val shotCost = costPerShot * coffee.shots
    val sugarCost = coffee.sugar.map(costForSugar).getOrElse(0.0)

    Charge(shotCost + milkCost + sugarCost)
  }

    trait MilkDb[F[_]] {
      def priceForSoy: F[Double]
      // Imagine other things here
    }
}

trait CustomerInteraction[F[_]] {
  def getCoffeeForOrder: F[Coffee]
  def confirmCostFor(coffee: Coffee, charge: Charge): F[Boolean]
  def handOverCoffee(coffee: Coffee): F[Unit]
}

trait PaymentGateway[F[_]] {
  def singlePayment(charge: Charge): F[Either[String, Receipt]]
}

trait CoffeeMachine[F[_]] {
  def makeCoffee(coffee: Coffee): F[Coffee]
}
