
import Bank.Acc

import scala.io.StdIn.readLine
import scala.runtime.Nothing$

object Main extends App {
  val help2 = "(e)xit \n(w)ithdraw \n(d)eposit \n(t)ransfer \n(b)alance"
  val help1 = "(l)ogin \n(e)xit \n(c)reate account \n(d)elete Account"


  var currentParser: Parser = LoginParser
  var help = help1
  while (true) {
    println(help)
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
      if (x > 0)
        if (AccountManager.user.getBalance - x >= 0)
          AccountManager.user = AccountManager.user.withdraw(x)
          println(s"Current balance: ${AccountManager.user.getBalance}")
        else
          Failure("not enough Balance").execute()
      else
        Failure("Must be positive number").execute()

    }
  }

  case class Deposit() extends Command {
    override def execute(): Unit = {
      println("How much money do you want do deposit?")
      val x = readLine().toInt
      if (x < 0)
        AccountManager.user = AccountManager.user.deposit(x)
        println(s"Current balance: ${AccountManager.user.getBalance}")
      else
        Failure("Must be positive number").execute()
    }
  }

  case class Transfer() extends Command {
    override def execute(): Unit = ???
  }

  case class Balance() extends Command {
    override def execute(): Unit = println(s"Current Balance: ${AccountManager.user.getBalance}" + "$")
  }

  case class Failure(msg: String) extends Command {
    override def execute(): Unit = println("Error: " + msg)
  }

  case class Login() extends Command {
    override def execute(): Unit = {
      println("Please enter your Bank-Information's: <IBAN> <PIN>")
      val x = readLine().split(" ")
      if (x.length == 2)
        val acc = Bank.login(x(0), x(1))
        if (acc.isDefined)
          currentParser = AccountManager
          AccountManager.user = acc.get
          help = help2
        else Failure("Incorrect credentials").execute()
      else
        Failure("Invalid arguments").execute()
    }
  }

  case class CreateAccount() extends Command {
    override def execute(): Unit = {
      println("Enter PIN: 4 Digits")
      val x = readLine()
      try {
        val z = x.toInt
        if (z > 999 && z <= 9999)
          val success: Boolean = Bank.createAccount((Bank.listOfAccount.size + 1).toString, x)
          if (success)
            println("Account has been created")
            println(s"Your credentials are: ${Bank.listOfAccount.size} , $x")
          else
            throw new Exception()
        else throw new IllegalStateException()
      } catch {
        case e: IllegalStateException => Failure("PIN must be 4 Digits!").execute()
        case _ => Failure("Something went wrong please try again").execute()
      }
    }
  }

  case class DeleteAccount() extends Command {
    override def execute(): Unit = {
      println("Please enter your credentials: <IBAN> <PIN>")
      val x = readLine().split(" ")
      if (x.length == 2)
        val success = Bank.removeAccount(x(0), x(1))
        if (success)
          println("Account has been deleted")
        else
          Failure("Incorrect credentials").execute()
      else
        Failure("Invalid Arguments").execute()
    }
  }


}
