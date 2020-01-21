package afcens.resolver

import scala.language.implicitConversions

/** Ensemble type.
  *
  * A collection of functionality and DSL commands for use in the policy definitions.
  * The following functionality is provided:
  * <ul>
  * <li>Setting the ensemble name
  * <li>Configuring an utility expression
  * <li>Registering sub-ensembles
  * <li>Registering roles
  * <li>Specifying security actions
  * <li>Specifying constraints
  * <li>Specifying a situation predicate
  * <li>Common implicit conversions
  * </ul>
  *
  * See documentation of constituent traits for details.
  */
trait Ensemble
    extends Initializable
    with WithName
    with WithDescription
    with WithSelectionStatus
    with WithEnsembleGroups
    with WithRoles
    with WithUtility
    with WithActions
    with WithConstraints
    with CommonImplicits {

  /** User-specified situation predicate function. */
  private var _situationFun: () => Boolean = null

  /** Specify a situation predicate as an expression.
    *
    * When the situation predicate evaluates to `false`, the ensemble will be
    * deactivated, its constraints will not apply and it will have no members.
    *
    * If the ensemble is registered with `rules`, and, the situation predicate evaluates
    * to `true`, its constraints must be satisfied in order to find a valid solution.
    *
    * @param cond Situation predicate expression
    */
  def situation(cond: => Boolean): Unit = {
    _situationFun = cond _
  }

  /** Check if the situation predicate applies.
    *
    * A wrapper around [[_situationFun]], allowing situation predicate to be unspecified.
    *
    * @return value of the situation predicate, or `true` if it is unset.
    */
  private[resolver] def _isInSituation: Boolean = {
    if (_situationFun != null)
      _situationFun()
    else
      true
  }

  override def toString: String = s"<Ensemble:$name>"

  def describe: String =
    s"""Ensemble "$name":${Utils.renderContent((_roles ++ _ensembleGroups).map(_.describe))}"""


  /** Convert an `EnsembleGroup` to its members.
    *
    * Facilitates the following usage:
    * {{{
    *   val enses = rules(collectionOfEnsembles)
    *   constraint { enses.map(_.someRole).allDisjoint }
    * }}}
    *
    * The `map()` method would not work on `enses`, as it is an `EnsembleGroup`, not a collection.
    */
  implicit def ensembleGroupToMembers[E <: Ensemble](group: EnsembleGroup[E]): Iterable[E] =
    group.allMembers
}
