#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

sbt \
  -J-Xms2g \
  -J-Xmx2g \
  -J-XX:+AlwaysPreTouch \
  -J-XX:+UseParallelGC \
  "dcelJVM/Test/runMain io.github.scala_tessella.dcel.benchmark.UniformityBenchmark --mode=stable $*"
