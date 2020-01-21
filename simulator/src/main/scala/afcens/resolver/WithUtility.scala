package afcens.resolver

import InitStages.InitStages

/** Ensemble part for handling utility expressions
  *
  * Provides a DSL function for setting the utility expression, and functionality that
  * collects utilities from sub-ensembles.
  */
trait WithUtility extends Initializable

  /* This is here solely for the sake of enforcing that WithUtility is mixes after WithRoles and WithEnsembleGroups.
     Thanks to this, the utility is initialized from leaves to root.
   */
  with WithRoles with WithEnsembleGroups {
  this: WithSelectionStatus with WithConstraints =>

  /** Configured utility function. */
  private var _utilityFun: Option[() => Integer] = None

  /** Cached result of the utility function.
    *
    * The result must be cached, because otherwise each run of the utility function
    * generates a new `IntVar`. Because the result is being registered for optimization
    * by the solver, producing new `IntVar`s can mean that the solver-registered
    * variable becomes stale.
    */
  private[resolver] var _utility: Option[Integer] = None

  /** Set the utility expression.
    *
    * `util` is an `Integer` expression that is only evaluated when the ensemble hierarchy
    * is initialized. This way, it is possible to declare utility at ensemble specification
    * time.
    *
    * @param util Utility expression
    */
  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  /** Utility variable of this ensemble.
    *
    * Does not include utilities of sub-ensembles.
    */
  def utility: Integer = _utility.getOrElse(_solverModel.IntegerInt(0))

  /** Utility value of the current ensemble in the current solution.
    *
    * Can only be used when a solution exists.
    *
    * @return utility of this ensemble, or zero if no utility expression was specified.
    */
  def solutionUtility: Int = _utility match {
    case Some(value) => value.asInt
    case None        => 0
  }

  /** Initialization hook.
    *
    * If a utility expression is specified, its value is cached as part of the rules
    * creation step.
    */
  override private[resolver] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _utility = _utilityFun.map(_.apply())
      case _ =>
    }
  }
}
