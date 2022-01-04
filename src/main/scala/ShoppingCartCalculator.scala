import ItemTypes.{Apple, ItemType, Orange}

object ShoppingCartCalculator {

  def calculate(items: String): BigDecimal =
    calculate(asItemTypes(items))

  private def asItemTypes(items: String): Seq[ItemType]
  = items.split(',').filterNot(_.isBlank).map(ItemTypes.resolve).toSeq

  private def calculate(items: Seq[ItemType]): BigDecimal = {
    val (costOfApplesWithOffer, remainingNumberOfApples) = applyAppleOffer(items)
    val (costOfOrangesWithOffer, remainingNumberOfOranges) = applyOrangeOffer(items)

    val costOfApplesInPence = costOfApplesWithOffer + (remainingNumberOfApples * Apple.getCostInPence)
    val costOfOrangesInPence = costOfOrangesWithOffer + (remainingNumberOfOranges * Orange.getCostInPence)

    BigDecimal((costOfApplesInPence + costOfOrangesInPence) / 100.0)
  }

  /**
   * Offer: Buy one apple, get one free
   */
  private def applyAppleOffer(items: Seq[ItemType]): (Int, Int) = {
    val originalNumber = items.count(_ == Apple)
    val costPerAppliedOfferInPence = Apple.getCostInPence
    val numberPerAppliedOffer = 2
    val costOfAllAppliedOffers = (originalNumber / numberPerAppliedOffer) * costPerAppliedOfferInPence
    val remainingNumber = originalNumber % numberPerAppliedOffer
    (costOfAllAppliedOffers, remainingNumber)
  }

  /**
   * Buy three oranges, and get one free
   */
  private def applyOrangeOffer(items: Seq[ItemType]): (Int, Int) = {
    val originalNumber = items.count(_ == Orange)
    val costPerAppliedOfferInPence = Orange.getCostInPence * 2
    val numberPerAppliedOffer = 3
    val costOfAllAppliedOffers = (originalNumber / numberPerAppliedOffer) * costPerAppliedOfferInPence
    val remainingNumber = originalNumber % numberPerAppliedOffer
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
