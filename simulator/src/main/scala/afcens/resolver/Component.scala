package afcens.resolver

/** Component base class. */
trait Component extends WithName with WithDescription with Notifiable {
  override def toString: String = s"""Component "${name}""""

  def describe: String = toString
}
