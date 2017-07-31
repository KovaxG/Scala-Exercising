package CandyMachine

import Chapters.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine (locked: Boolean, candies: Int, coins: Int)

object Machine {
  type State[S, A] = S => (A, S)
  type Coins = Int
  type Candies = Int

  // I am cheating a bit, because the output tuple is contained in the machine, so I don't bother with it.
  def simulateMachine(inputs: List[Input]): State[Machine, (Coins, Candies)] =

    inputs.foldLeft(unit(0, 0): State[Machine, (Coins, Candies)])((acc, e) => initialMachine => {
      val (_, machine) = acc(initialMachine)

      val newMachine: Machine = machine match {
        case Machine(a, 0, b) => Machine(a, 0, b)

        case Machine(true, candies, coins) if candies > 0 && e == Coin => Machine(locked = false, candies, coins + 1)
        case Machine(true, candies, coins)  => Machine(locked = true,  candies, coins)

        case Machine(false, candies, coins) if e == Turn               => Machine(locked = true,  candies - 1, coins)
        case Machine(false, candies, coins) => Machine(locked = false, candies, coins)
      }
      ((newMachine.coins, newMachine.candies), newMachine)
    })
}
