import ItemTypes.ItemType

object ShoppingCartCalculator {

  def calculate(items: Seq[ItemType]): Int =
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
}
