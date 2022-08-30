
import Bank.Acc

import scala.io.StdIn.readLine
import scala.runtime.Nothing$
import scala.util.Random

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
      //TODO: implement Change Pin
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
      try {
        val x = readLine().toInt
        if (x > 0)
          if (AccountManager.user.getBalance - x >= 0)
            AccountManager.user = AccountManager.user.withdraw(x)
            println(s"Current balance: ${AccountManager.user.getBalance}")
          else
            Failure("not enough Balance").execute()
        else
          Failure("Must be positive number").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
    }
  }

  case class Deposit() extends Command {
    override def execute(): Unit = {
      println("How much money do you want do deposit?")
      try {
        val x = readLine().toInt
        if (x > 0)
          AccountManager.user = AccountManager.user.deposit(x)
          println(s"Current balance: ${AccountManager.user.getBalance}")
        else
          Failure("Must be positive number").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
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

  case class Success(msg: String) extends Command {
    override def execute(): Unit = println("Success: " + msg)
  }

  case class Login() extends Command {
    override def execute(): Unit = {
      println("Please enter your Bank-Information's: <IBAN> <PIN>")
      val x = readLine().split(" ")
      try {
        val acc = Bank.login(x(0), x(1))
        if (acc.isDefined)
          currentParser = AccountManager
          AccountManager.user = acc.get
          help = help2
        else Failure("Incorrect credentials").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
    }
  }

  case class CreateAccount() extends Command {
    override def execute(): Unit = {
      var iban: Int = -1
      while (iban == -1 || checkIBAN(iban)) {
        iban = generateIBAN()
      }
      val pin = generatePIN()
      val success = Bank.createAccount(iban.toString, pin)
      if (success)
        Success("Account has been created")
        println(s"Your Credentials are: $iban $pin")
      else Failure("Something went wrong, pleas try again")
    }
  }

  case class DeleteAccount() extends Command {
    override def execute(): Unit = {
      println("Please enter your credentials: <IBAN> <PIN>")
      val x = readLine().split(" ")
      if (x.length == 2)
        val success = Bank.removeAccount(x(0), x(1))
        if (success)
          Success("Account has been deleted").execute()
        else
          Failure("Incorrect credentials").execute()
      else
        Failure("Invalid Arguments").execute()
    }
  }

  def generateIBAN(): Int = {
    val x: Int = new Random().between(1, 9999)
    println(x)
    Bank.listOfAccount.map({
      case (id, _) if (id._1.toInt != x) => x
      case _ => -1
    })
    x
  }

  def generatePIN(): String = {
    new Random().between(1000, 9999).toString
  }

  def checkIBAN(iban: Int): Boolean = {
    Bank.listOfAccount.find(_._1._1.toInt == iban).isDefined
  }
}

