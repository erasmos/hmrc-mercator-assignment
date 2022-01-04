import ItemTypes.{Apple, Orange}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShoppingCartCalculatorTest extends AnyWordSpec with Matchers with BeforeAndAfterEach {

  override protected def beforeEach(): Unit = {
    //
  }

  "When calculating the total cost of a shopping cart, we should" when {
    "fail" when {
      "there is an unrecognised item" in {
        pending
      }
    }
    "succeed, returning the expected total price" when {
      "there are" when {
        "no items" in {
          val totalValueInPence = ShoppingCartCalculator.calculate(Seq.empty)

          totalValueInPence shouldBe 0
        }
        "Three apples and one orange" in {
          val totalValueInPence = ShoppingCartCalculator.calculate(Seq(Apple, Apple, Orange, Apple))

          totalValueInPence shouldBe 205
        }
        "One apples and three orange" in {
          val totalValueInPence = ShoppingCartCalculator.calculate(Seq(Apple, Orange, Orange, Orange))

          totalValueInPence shouldBe 135
        }
        "One apple" in {
          val totalValueInPence = ShoppingCartCalculator.calculate(Seq(Apple))

          totalValueInPence shouldBe 60
        }
        "One orange" in {
          val totalValueInPence = ShoppingCartCalculator.calculate(Seq(Orange))

          totalValueInPence shouldBe 25
        }
      }

    }
  }
}
