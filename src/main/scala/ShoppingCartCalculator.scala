import ItemTypes.{Apple, Banana, ItemType, Orange}
import Offers.BuyOneAppleGetOneFree.process

object ShoppingCartCalculator {

  def calculate(items: String): BigDecimal =
    calculate(asItemTypes(items))

  private def asItemTypes(items: String): Seq[ItemType]
  = items.split(',').filterNot(_.isBlank).map(ItemTypes.resolve).toSeq

  private def calculate(items: Seq[ItemType]): BigDecimal = {
    val totalCostInPence = Offers.process(items).map(_.getCostInPence).sum
    BigDecimal(totalCostInPence / 100.0)
  }

}

object ItemTypes {

  sealed trait ItemType {
    def getCostInPence: Int
  }

  case object Apple extends ItemType {
    override def getCostInPence: Int = 60
  }

  case object Orange extends ItemType {
    override def getCostInPence: Int = 25
  }

  case object Banana extends ItemType {
    override def getCostInPence: Int = 20
  }

  def resolve(itemTypeCode: String): ItemType =
    itemTypeCode.trim.toUpperCase match {
      case "A" => Apple
      case "B" => Banana
      case "O" => Orange
      case _ => throw new UnknownItemTypeException(itemTypeCode)
    }
}

class UnknownItemTypeException(itemTypeCode: String) extends RuntimeException(s"Unknown item type: [$itemTypeCode]")

object Offers {

  val offers = Seq(BuyOneAppleGetOneFree, BuyTwoOrangesGetOneFree, BuyOneBananaGetOneFree)

  def process(items: Seq[ItemType]): Seq[ItemType] =
    offers.foldLeft(items)((items, offer) => offer.process(items))

  trait Offer {
    def process(items: Seq[ItemType]): Seq[ItemType]
  }

  trait BuySomeGetOneOfSameFree extends Offer {
    def process(paidItemType: ItemType, freeItemType: ItemType, items: Seq[ItemType], minItemsInSet: Int): Seq[ItemType] = {
      val numberOfItems = items.count(_ == paidItemType)
      val numberOfSets = numberOfItems / minItemsInSet
      val freeItems = Seq.fill(numberOfSets)(freeItemType)
      items diff freeItems
    }
  }

  case object BuyOneAppleGetOneFree extends BuySomeGetOneOfSameFree {
    override def process(items: Seq[ItemType]): Seq[ItemType] = process(Apple, Apple, items, 2)
  }

  case object BuyTwoOrangesGetOneFree extends BuySomeGetOneOfSameFree {
    override def process(items: Seq[ItemType]): Seq[ItemType] = process(Orange, Orange, items, 3)
  }

  case object BuyOneBananaGetOneFree extends BuySomeGetOneOfSameFree {
    override def process(items: Seq[ItemType]): Seq[ItemType] = process(Banana, Banana, items, 2)
  }

//  case object BuyOneAppleAndOneBananaCheapestOneFree extends Offer {
//    override def process(items: Seq[ItemType]): Seq[ItemType] = {
//      val numberOfBanana = items.count(_ == Banana)
//      val numberOfPairsOfBanana = numberOfBanana / 2
//      val freeBananas = Seq.fill(numberOfPairsOfBanana)(Banana)
//      items diff freeBananas
//    }
//  }

}
