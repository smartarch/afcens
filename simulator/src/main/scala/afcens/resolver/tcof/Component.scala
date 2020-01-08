package afcens.resolver.tcof

import afcens.resolver.tcof.InitStages.InitStages
import afcens.resolver.tcof.Utils._
import org.chocosolver.solver.Model

import scala.collection.mutable

trait Component extends WithName with Notifiable {
  override def toString: String =
    s"""Component "$name""""
}
