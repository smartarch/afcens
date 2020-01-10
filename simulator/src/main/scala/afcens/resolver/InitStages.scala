package afcens.resolver

/** Initialization stages.
  *
  * See thesis section 6.2.6 for detailed description.
  */
private[resolver] object InitStages extends Enumeration {
  type InitStages = Value

  /** Initialization stages.
    *
    * @see [[Initializable._init()]]
    */
  val ConfigPropagation, VarsCreation, RulesCreation = Value
}
