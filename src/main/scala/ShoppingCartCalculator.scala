import ItemTypes.{Apple, ItemType, Orange}

object ShoppingCartCalculator {

  def calculate(items: String): Int =
    calculate(asItemTypes(items))

  private def asItemTypes(items: String): Seq[ItemType]
  = items.split(',').filterNot(_.isBlank).map(ItemTypes.resolve).toSeq

  private def calculate(items: Seq[ItemType]): Int = {
    val (costOfApplesWithOffer, remainingNumberOfApples) = applyAppleOffer(items)
    val (costOfOrangesWithOffer, remainingNumberOfOranges) = applyOrangeOffer(items)

    val costOfApples = costOfApplesWithOffer + (remainingNumberOfApples * Apple.getCostInPence)
    val costOfOranges = costOfOrangesWithOffer + (remainingNumberOfOranges * Orange.getCostInPence)

    costOfApples + costOfOranges
  }

  /**
   * Offer: Buy one apple, get one free
   */
  private def applyAppleOffer(items: Seq[ItemType]): (Int, Int) = {
    val originalNumber = items.count(_ == Apple)
    val costPerAppliedOfferInPence = 60
    val costOfAllAppliedOffers = (originalNumber / 2) * costPerAppliedOfferInPence
    val remainingNumber = originalNumber % 2
    (costOfAllAppliedOffers, remainingNumber)
  }

  /**
   * Buy three oranges, and get one free
   */
  private def applyOrangeOffer(items: Seq[ItemType]): (Int, Int) = {
    val originalNumber = items.count(_ == Orange)
    val costPerAppliedOfferInPence = 50
    val costOfAllAppliedOffers = (originalNumber / 3) * costPerAppliedOfferInPence
    val remainingNumber = originalNumber % 3
    (costOfAllAppliedOffers, remainingNumber)
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

  def resolve(itemTypeCode: String): ItemType =
    itemTypeCode.trim.toUpperCase match {
      case "A" => Apple
      case "O" => Orange
      case _ => throw new UnknownItemTypeException(itemTypeCode)
    }
}

class UnknownItemTypeException(itemTypeCode: String) extends RuntimeException(s"Unknown item type: [$itemTypeCode]")
