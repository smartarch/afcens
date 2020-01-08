package afcens.resolver.tcof

trait WithID {
  type IDType

  def id: IDType
}
