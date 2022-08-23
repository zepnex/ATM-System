
import Bank.Acc

import scala.io.StdIn.readLine
import scala.runtime.Nothing$

object Main extends App {
  val help = "(e)xit \n(w)ithdraw \n(d)eposit \n(t)ransfer \n(b)alance"
  val help2 = "(l)ogin \n(e)xit \n(c)reate account \n(d)elete Account"


  var currentParser: Parser = LoginParser
  while (true) {
    println(help2)
    currentParser(readLine()).execute()

  }

  trait Parser {
    def apply(x: String): Command
  }

  object AccountManager extends Parser {
    var user: Bank.Acc = _

    def apply(x: String): Command = x match {
      case "e" => Exit()
      case "w" => Withdraw()
      case "d" => Deposit()
      case "t" => Transfer()
      case "b" => Balance()
      case _ => Failure("Unknown Command")

    }
  }

  object LoginParser extends Parser {
    def apply(x: String): Command = x match
      case "l" => Login()
      case "e" => Exit()
      case "c" => CreateAccount()
      case "d" => DeleteAccount()
      case _ => Failure("Unknown Command")
  }

  trait Command {
    def execute(): Unit
  }

  case class Exit() extends Command {
    override def execute(): Unit = sys.exit()
  }

  case class Withdraw() extends Command {
    override def execute(): Unit = {
      println("How much money do you want do withdraw?")
      val x = readLine().toInt
      if (AccountManager.user.getBalance - x > 0)
        AccountManager.user = AccountManager.user.withdraw(x)
        println(s"Current balance: ${AccountManager.user.getBalance}")
      else
        Failure("not enough Balance").execute()
    }
  }

  case class Deposit() extends Command {
    override def execute(): Unit = {
      println("How much money do you want do deposit?")
      val x = readLine().toInt
      AccountManager.user = AccountManager.user.deposit(x)
      println(s"Current balance: ${AccountManager.user.getBalance}")
    }
  }

  case class Transfer() extends Command {
    override def execute(): Unit = ???
  }

  case class Balance() extends Command {
    override def execute(): Unit = AccountManager.user.getBalance
  }

  case class Failure(msg: String) extends Command {
    override def execute(): Unit = sys.error(s"Error $msg")
  }

  case class Login() extends Command {
    override def execute(): Unit = {
      println("Please enter your Bank-Information's: <IBAN> <PIN>")
      val x = readLine().split(" ")
      val acc = Bank.login(x(0), x(1))
      // TODO: If successfully change parser
      if (acc.isDefined)
        currentParser = AccountManager
        AccountManager.user = acc.get
      else println("Sorry credentials were wrong")
    }
  }

  case class CreateAccount() extends Command {
    override def execute(): Unit = ???
  }

  case class DeleteAccount() extends Command {
    override def execute(): Unit = ???
  }


}
