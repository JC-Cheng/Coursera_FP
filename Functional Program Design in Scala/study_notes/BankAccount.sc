object obMode{
  trait Subscriber {
    def handler(publisher: Publisher): Unit
  }

  trait  Publisher {
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscriber: Subscriber): Unit = subscribers += subscriber
    def unsubscribe(subscriber: Subscriber): Unit = subscribers -= subscriber
    def publish(): Unit = subscribers.foreach(_.handler(this))

  }


  class BankAccount extends Publisher {
    private var balance = 0
    def currentBalance: Int = balance

    def deposit(amt: Int): Unit = {
      if (amt > 0) balance += amt
      publish()
    }

    def withdraw(amt: Int): Unit = {
      if (amt > 0 && amt < balance) {
        balance -= amt
        publish()
      } else throw new Error("Insufficient Funds")
    }
  }


  class Consolidator(observed: List[BankAccount]) extends Subscriber {
    // initialization
    observed.foreach(_.subscribe(this))

    private var total: Int = _ // uninitialized
    compute()

    private def compute(): Unit = {
      total = observed.map(_.currentBalance).sum
    }

    def handler(pub: Publisher): Unit = {
      print("Updating Balance...")
      compute()
    }

    def totalBalance = total
  }
}

val acc_1 = new obMode.BankAccount
val acc_2 = new obMode.BankAccount
val bank = new obMode.Consolidator(List(acc_1, acc_2))

bank totalBalance

acc_1 deposit 20
acc_2 deposit 10

bank totalBalance

acc_2 deposit 20
acc_1 withdraw 15

bank totalBalance

