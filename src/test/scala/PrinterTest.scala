import org.scalatest.{FreeSpec, Matchers}

class PrinterTest extends FreeSpec with Matchers {

  "Ouputs hello world" in {
    Printer.hello() shouldBe "Hello World"
  }

}
