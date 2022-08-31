
import Bank.Acc

import scala.io.StdIn.readLine
import scala.runtime.Nothing$
import scala.util.Random

object Main extends App {
  val help2 = "(e)xit \n(w)ithdraw \n(d)eposit \n(t)ransfer \n(b)alance \n(l)ogout"
  val help1 = "(l)ogin \n(e)xit \n(c)reate account \n(d)elete Account"

  var currentParser: Parser = LoginParser
  var help = help1

  while (true) {
    println(help)
    currentParser(readLine()).execute()
    println(Bank.listOfAccount)

  }

  trait Parser {
    def apply(x: String): Command
  }

  object AccountManager extends Parser {
    var user: Option[Acc] = _

    def apply(x: String): Command = x.replace(" ", "") match {
      case "e" => Exit()
      case "w" => Withdraw()
      case "d" => Deposit()
      case "t" => Transfer()
      case "b" => Balance()
      case "l" => Logout()
      case "c" => ChangePin()
      case _ => Failure("Unknown Command")
      //TODO: implement Change Pin
    }
  }

  object LoginParser extends Parser {
    def apply(x: String): Command = x.replace(" ", "") match
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
          var acc = AccountManager.user.get
          if (acc.getBalance - x >= 0)
            AccountManager.user = acc.withdraw(x)
            acc = AccountManager.user.get
            Success(s"Current balance: ${acc.getBalance}").execute()
          else
            Failure("not enough Balance").execute()
        else
          Failure("Must be positive number").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
    }
  }

  //TODO rewrite:
  case class Deposit() extends Command {
    override def execute(): Unit = {
      println("How much money do you want do deposit?")
      try {
        val x = readLine().toInt
        if (x > 0)
          var acc = AccountManager.user.get
          AccountManager.user = acc.deposit(x)
          acc = AccountManager.user.get
          Success(s"Current balance: ${acc.getBalance}").execute()
        else
          Failure("Must be positive number").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
    }

  }

  case class Transfer() extends Command {
    override def execute(): Unit = {
      println("Please enter the destination IBAN")
      try {
        val iban = readLine().toInt
        val acc = AccountManager.user.get
        if (iban == acc.id.toInt) {
          Failure("Can't transfer money to sender account").execute()
          return
        }
        println("Please enter the amount you want to transfer")
        val amount = readLine().toInt
        if (!checkIBAN(iban))
          Failure("Wrong IBAN or amount less then 1$").execute()
        else if (amount < 1) {
          Failure("Amount must be greater then $0").execute()
        } else if (amount > AccountManager.user.get.getBalance) {
          Failure("Not enough balance").execute()
        } else {
          val destination = Bank.listOfAccount.find(_._1._1.toInt == iban).get._2
          val newAcc = acc.withdraw(amount)
          AccountManager.user = newAcc
          println(AccountManager.user.get)
          destination.deposit(amount)
          Success(s"$amount has been transferred to ${destination.id}").execute()
        }
      } catch {
        case _ => Failure("Invalid Argument").execute()
      }
    }
  }

  case class Balance() extends Command {
    override def execute(): Unit = Success(s"Current Balance: ${AccountManager.user.get.getBalance}" + "$").execute()
  }

  case class Failure(msg: String) extends Command {
    override def execute(): Unit = println(s"\u001B[31m[Error]: $msg\u001B[0m")
  }

  case class Success(msg: String) extends Command {
    override def execute(): Unit = println(s"\u001B[32m[Success]: $msg\u001B[0m")
  }

  case class Login() extends Command {
    override def execute(): Unit = {
      println("Please enter your Bank-Information's: <IBAN> <PIN>")
      val x = readLine().split(" ")
      try {
        val acc = Bank.login(x(0), x(1))
        if (acc.isDefined)
          currentParser = AccountManager
          AccountManager.user = acc
          help = help2
          Success(s"Logged in as: ${AccountManager.user.get.id}")
        else Failure("Incorrect credentials").execute()
      } catch {
        case _ => Failure("Invalid arguments").execute()
      }
    }
  }

  case class Logout() extends Command {
    override def execute(): Unit = {
      currentParser = LoginParser
      AccountManager.user = None
      help = help1
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
        Success("Account has been created").execute()
        Success(s"Your Credentials are: $iban $pin").execute()
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

  //TODO: ask one more time for credentials before changing PIN
  case class ChangePin() extends Command {
    override def execute(): Unit = {
      println("Please enter your new PIN")
      try {
        val newPin = readLine().toInt
        if(newPin == AccountManager.user.get.pin.toInt)
          Failure("new PIN cannot be the same as old PIN").execute()
        else if(newPin<1000 || newPin>9999)
          Failure("PIN must be 4 digits only").execute()
        else
          val newAcc = AccountManager.user.get.changePin(newPin.toString)
          AccountManager.user = newAcc
          println(newAcc.hashCode())
          Success("PIN has been changed")
      }catch {
        case _ => Failure("PIN must be 4 digits only").execute()
      }
    }
  }

  def generateIBAN(): Int = {
    val x: Int = new Random().between(1, 999)
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
    Bank.listOfAccount.exists(_._1._1.toInt == iban)
  }
}

