package afcens.resolver

/** Name provider. */
trait WithName {
  /** Name value. */
  private var _name = Utils.randomName

  /** Name setter. */
  def name(nm: String): Unit = _name = nm

  /** Name getter. */
  def name: String = _name
}
