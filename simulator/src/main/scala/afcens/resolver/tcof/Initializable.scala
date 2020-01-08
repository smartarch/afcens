package afcens.resolver.tcof

import afcens.resolver.tcof.InitStages.InitStages

trait Initializable {
  private[tcof] def _init(stage: InitStages, config: Config): Unit = {
  }
}



