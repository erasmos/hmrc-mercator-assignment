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
        the[UnknownItemTypeException] thrownBy {
          ShoppingCartCalculator.calculate("A,X,O")
        } should have message "Unknown item type: [X]"
      }
    }
    "succeed, returning the expected total price" when {
      "there are" when {
        "no items" in {
          val totalValueInPence = ShoppingCartCalculator.calculate("")

          totalValueInPence shouldBe 0
        }
        "items with different casing" in {
          val totalValueInPence = ShoppingCartCalculator.calculate("A,a,O,A")

          totalValueInPence shouldBe 205
        }
        "items with padding" in {
          val totalValueInPence = ShoppingCartCalculator.calculate("A,O,  O  , O  ")

          totalValueInPence shouldBe 135
        }
        "items with upper-casing, without padding" in {
          val totalValueInPence = ShoppingCartCalculator.calculate("A,O")

          totalValueInPence shouldBe 85
        }
      }

    }
  }
}
