package dispatch

object O {
  var i = 0
  @noinline def m(): Boolean = {
    i += 1
    i < 100000
  }
}

class StaticCallBenchmark extends benchmarks.Benchmark[Unit] {
  @noinline override def run(): Unit = {
    O.i = 0
    while (O.m()) {}
  }

  override def check(v: Unit): Boolean = true
}
