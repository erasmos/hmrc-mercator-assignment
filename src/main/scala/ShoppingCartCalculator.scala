import ItemTypes.ItemType

object ShoppingCartCalculator {

  def calculate(items: String): Int =
    calculate(asItemTypes(items))

  private def asItemTypes(items: String): Seq[ItemType]
    = items.split(',').filterNot(_.isBlank).map(ItemTypes.resolve).toSeq

  private def calculate(items: Seq[ItemType]): Int =
    items.map(_.getCostInPence).sum
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
