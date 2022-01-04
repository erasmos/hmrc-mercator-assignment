import org.scalatest.BeforeAndAfterEach
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ShoppingCartCalculatorTest extends AnyFreeSpec with Matchers with BeforeAndAfterEach {

  "When calculating the total cost of a shopping cart, we should" - {
    "fail when" - {
      "there is an unrecognised item" in {
        the[UnknownItemTypeException] thrownBy {
          ShoppingCartCalculator.calculate("A,X,O")
        } should have message "Unknown item type: [X]"
      }
    }
    "succeed, returning the expected total price, when" - {
      "there are" - {
        "no items" in {
          val totalValue = ShoppingCartCalculator.calculate("")

          totalValue shouldBe BigDecimal(0)
        }
        "items for which there" - {
          "are no applicable offers" - {
            "with different casing" in {
              val totalValue = ShoppingCartCalculator.calculate("a,o")

              totalValue shouldBe BigDecimal(0.85)
            }
            "with padding" in {
              val totalValue = ShoppingCartCalculator.calculate("A,O,  O")

              totalValue shouldBe BigDecimal(1.10)
            }
            "with upper-casing, without padding" in {
              val totalValue = ShoppingCartCalculator.calculate("A,O")

              totalValue shouldBe BigDecimal(0.85)
            }
          }
          "is a single offer applied once, namely" - {
            "buy one apple, and get one free" in {
              val totalValue = ShoppingCartCalculator.calculate("A,A")

              totalValue shouldBe BigDecimal(0.60)
            }
            "buy three oranges, and get one free" in {
              val totalValue = ShoppingCartCalculator.calculate("O,O,O")

              totalValue shouldBe BigDecimal(0.50)
            }
          }
          "is a single offer applied multiple times, namely" - {
            "buy one apple, and get one free" in {
              val totalValue = ShoppingCartCalculator.calculate("A,A,A,A,A")

              totalValue shouldBe BigDecimal(1.80)
            }
            "buy three oranges, and get one free" in {
              val totalValue = ShoppingCartCalculator.calculate("O,O,O,O,O,O,O")

              totalValue shouldBe BigDecimal(1.25)
            }
          }
          "are multiple applicable offers applied multiple times" in {
            val totalValue = ShoppingCartCalculator.calculate("A,O,O,A,A,A,O,O,O,O")

            totalValue shouldBe BigDecimal(2.20)
          }

        }
      }

    }
  }
}
