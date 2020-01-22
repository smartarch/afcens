package afcens.resolver

import scala.collection.mutable

/** Ensemble part for handling security actions.
  *
  * Provides DSL functions that create security actions and notifications, and
  * functionality for collecting the actions from sub-ensembles.
  */
trait WithActions {
  this: WithEnsembleGroups =>

  /** Local type alias for roles. */
  private type Role = MemberGroup[Component]

  /** List of generators of actions. */
  private[resolver] val _actions = mutable.ListBuffer.empty[() => Unit]

  /** Collect actions from this ensemble and all its sub-ensembles. */
  private[resolver] def _collectActions(): Iterable[() => Unit] = {
    _actions.toList ++ _ensembleGroups
      .flatMap(_.selectedMembers)
      .flatMap(_._collectActions())
  }

  /** Adds an action to be executed if ensemble is establish.
    *
    * @param act Code of the action execute if the ensemble is instantiated
    */
  def tasks(act: => Unit): Unit = {
    _actions += (act _)
  }
}
