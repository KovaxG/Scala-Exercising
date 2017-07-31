package CandyMachine

import org.scalatest.{FreeSpec, Matchers}

class MachineTest extends FreeSpec with Matchers {
  "Inserting a coin into a locked machine will cause it to unlock if there's any candy left" in {
    val (_, mach) =  Machine.simulateMachine(List(Coin))(new Machine(true, 10, 10))
    mach shouldBe new Machine(false, 10, 11)

    val (_, mach2) =  Machine.simulateMachine(List(Coin))(new Machine(true, 0, 10))
    mach2 shouldBe new Machine(true, 0, 10)
  }

  "Turning the knob on an unlocked machine will cause it to dispense candy and become locked" in {
    val (_, mach) =  Machine.simulateMachine(List(Turn))(new Machine(false, 10, 10))
    mach shouldBe new Machine(true, 9, 10)
  }

  "Tunring the knob on a locked machine or inserting a coin into an unlocked machine does nothing" in {
    val (_, mach) =  Machine.simulateMachine(List(Turn))(new Machine(true, 10, 10))
    mach shouldBe new Machine(true, 10, 10)

    val (_, mach2) =  Machine.simulateMachine(List(Coin))(new Machine(false, 10, 10))
    mach2 shouldBe new Machine(false, 10, 10)
  }

  "A machine that's out of candy ignores all inputs" in {
    val imach = new Machine(true, 0, 10)
    val (_, mach) =  Machine.simulateMachine(List(Turn))(imach)
    mach shouldBe imach

    val imach2 = new Machine(false, 0, 10)
    val (_, mach2) =  Machine.simulateMachine(List(Turn))(imach2)
    mach2 shouldBe imach2

    val imach3 = new Machine(false, 0, 10)
    val (_, mach3) =  Machine.simulateMachine(List(Coin))(imach3)
    mach3 shouldBe imach3

    val imach4 = new Machine(true, 0, 10)
    val (_, mach4) =  Machine.simulateMachine(List(Coin))(imach4)
    mach4 shouldBe imach4
  }
}
