package afcens.resolver

import org.chocosolver.solver.Model

/** Root of a system of ensembles.
  *
  * Provides an interface to the solver, methods controlling the solving process,
  * access to the solution, and access to generated actions.
  *
  * @param builder Constructor expression of the ensemble hierarchy.
  * @tparam EnsembleType Concrete type of the root ensemble.
  */
class EnsembleSystem[EnsembleType <: Ensemble](builder: () => EnsembleType) {

  /** Reference to an instance of the ensemble hierarchy. */
  private var _solution: EnsembleType = _

  /** Reference to an utility value of the solution. */
  private var _utility: Option[Integer] = None

  /** Solver instance. */
  private var _solverModel: SolverModel = _

  /** Initialize the solution process.
    *
    * Generates a new instance of the ensemble hierarchy from the `builder` function,
    * clears the action lists, sets a solution time limit if specifed, initializes
    * the solver and the ensembles.
    *
    * A new solver instance must be created every time, otherwise it retains previously
    * posted constraints.
    *
    * A new instance of the ensemble hierarchy must be created every time, because the
    * ensemble objects often close over external values, which need to be refreshed.
    *
    * If any utility expression is found, the solver is configured to maximize it.
    * This must only be done if an utility expression exists. Otherwise, we could
    * configure the solver to maximize a constant, but then the first solution would
    * be optimal and we could not iterate over other possible solutions.
    *
    * @param limit Time limit in milliseconds. If a solving attempt takes more than
    *              `limit` ms, it is stopped.
    */
  def init(limit: Long = -1): Unit = {
    _solution = builder()
    _solverModel = new SolverModel
    val config = new Config(_solverModel)

    // initialize solution
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    // initialize root component
    _solverModel.arithm(_solution.isSelectedVar, "=", 1).post()
    _solverModel.post(_solution._buildConstraintsClause)

    // configure utility
    _utility = _solution._utility
    _utility match {
      case Some(config.solverModel.IntegerIntVar(utility)) =>
        _solverModel.setObjective(Model.MAXIMIZE, utility)
      case _ => // utility is constant or unset, so we ignore it
    }

    _solverModel.init()
    if (limit > -1) _solverModel.getSolver.limitTime(limit)
  }

  /** Utility value of latest solution.
    *
    * If no utility expressions were configured, the value is zero.
    */
  def solutionUtility: Int = _utility.map(_.asInt).getOrElse(0)

  /** Find next solution.
    *
    * Instructs the solver to find next solution, and makes it available for examining.
    *
    * @return true if a new solution was found, false otherwise.
    */
  def solve(): Boolean = {
    _solverModel.solveAndRecord()
  }

  /** Reference to the root ensemble instance. */
  def instance: EnsembleType = _solution

  /** True if a solution exists. */
  def exists: Boolean = _solverModel.exists

  /** Execute actions.
    *
    * If `commit()` is not called, no actions are executed.
    */
  def commit(): Unit = {
    val _actions = _solution._collectActions()
    println(_actions)
    _actions.foreach(action => action())
  }

  /** Resolve the policy.
    *
    * An all-in-one method to initialize the solver and find the optimal solution in
    * terms of utility. If no utility is specified, find the first solution.
    *
    * @param limit Time limit in milliseconds
    * @return true if a solution was found within the time limit, false otherwise.
    */
  def resolve(limit: Long = -1): Boolean = {
    init(limit)

    _utility match {
      // repeatedly solve to optimize objective
      case Some(_) => while (solve()) {}
      // solve and record first solution
      case None => solve()
    }
    if (exists) commit()
    return exists
  }
}

/** Policy companion object. */
object EnsembleSystem {

  /** Create a policy root.
    *
    * DSL function to register the root ensemble. Typical usage:
    * {{{
    *   class MyEnsemble extends Ensemble {
    *     /* ... */
    *   }
    *
    *   val system = EnsembleSystem(new Ensemble)
    * }}}
    *
    * @param builder Expression that creates an instance of the root ensemble
    * @tparam E Concrete type of the root ensemble
    * @return an object wrapping the ensemble hierarchy.
    */
  def apply[E <: Ensemble](builder: => E): EnsembleSystem[E] = new EnsembleSystem(builder _)
}
