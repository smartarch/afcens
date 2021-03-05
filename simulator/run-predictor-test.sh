#/bin/bash

predId=$1
sbt "runMain afcens.TraceRecordWithPredictor $predId 0 1000"

pushd traces/v2/$predId

rm -f eat-ticks.csv

for d in [0-9]*; do
  for f in $d/*.jsonl.gz; do
    zcat $f | tail -n 1 | jq .eatTicks >> eat-ticks.csv
  done
done

popd

