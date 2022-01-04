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
        "items for which there are" when {
          "no applicable offers" when {
            "with different casing" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("a,o")

              totalValueInPence shouldBe 85
            }
            "with padding" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("A,O,  O")

              totalValueInPence shouldBe 110
            }
            "with upper-casing, without padding" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("A,O")

              totalValueInPence shouldBe 85
            }
          }
          "a single offer applied once, namely" when {
            "buy one apple, and get one free" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("A,A")

              totalValueInPence shouldBe 60
            }
            "buy three oranges, and get one free" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("O,O,O")

              totalValueInPence shouldBe 50
            }
          }
          "a single offer applied multiple times, namely" when {
            "buy one apple, and get one free" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("A,A,A,A,A")

              totalValueInPence shouldBe 180
            }
            "buy three oranges, and get one free" in {
              val totalValueInPence = ShoppingCartCalculator.calculate("O,O,O,O,O,O,O")

              totalValueInPence shouldBe 125
            }
          }
          "multiple applicable offers applied multiple times" in {
            val totalValueInPence = ShoppingCartCalculator.calculate("A,O,O,A,A,A,O,O,O,O")

            totalValueInPence shouldBe 220
          }

        }
      }

    }
  }
}
