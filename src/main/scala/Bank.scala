import collection.mutable

object Bank {
  // val listOfAccount: Map[(String, String), Acc] = Map.empty
  val listOfAccount: mutable.Map[(String, String), Acc] = mutable.Map(("1", "1234") -> Acc("1", "1234", 20))

  def login(id: String, pin: String): Option[Acc] = {
    if (listOfAccount.contains((id, pin))) Some(listOfAccount((id, pin)))
    else None
  }

  def createAccount(id: String, pin: String): Boolean = {
    if (!listOfAccount.contains((id, pin)))
      listOfAccount += ((id, pin) -> Acc(id, pin, 0))
      true
    else false
  }

  def removeAccount(id: String, pin: String): Boolean = {
    if (listOfAccount.contains((id, pin)))
      listOfAccount -= ((id, pin))
      true
    else false
  }

  case class Acc(id: String, pin: String, balance: Int) {
    def withdraw(n: Int): Option[Acc] = {
      listOfAccount((id, pin)) = Acc(id, pin, balance - n)
      listOfAccount.get((id, pin))
    }

    def deposit(n: Int): Option[Acc] = {
      listOfAccount((id, pin)) = Acc(id, pin, balance + n)
      listOfAccount.get((id, pin))
    }

    def changePin(newPin: String): Option[Acc] = {
      removeAccount(id, pin)
      createAccount(id, newPin)
      println(listOfAccount.get((id, newPin)).hashCode())
      listOfAccount.get((id, newPin))
    }

    def getBalance: Int = balance
  }
}
