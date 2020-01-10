package afcens.resolver

/** Policy configuration object.
  *
  * Propagated as part of the initialization process. See [[Initializable._init()]] and
  * section 6.2.6 in the accompanying thesis.
  *
  * Currently only contains the CSP solver object, but can be extended in the future
  * to hold more configuration information.
  *
  * @param solverModel solver object
  */
private[resolver] class Config(val solverModel: SolverModel)
