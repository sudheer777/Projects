import java.io.File
import scala.io.StdIn.readLine
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object exercise2 {
  val rand = scala.util.Random

  class Ewallet(val customerId: String, var availableMoney: Double, var codePin: Int = 0) {
    if (codePin == 0) {
      codePin = rand.nextInt(99900) + 100
    }
    val overdraftFees: Int = 500
    val feesRate: Int = 3

    def verifieClient(idclient_verif : String, codepin_verif : Int) : Boolean = {
      customerId == idclient_verif && codePin == codepin_verif
    }

    def operationDebit(montantadebiter : Double) : Double = {
      val transactionfees = (montantadebiter * feesRate) /100
      val total = montantadebiter + transactionfees
      if (availableMoney + overdraftFees >= total) {
        availableMoney -= total
        availableMoney
      } else Double.NaN
    }

    def operationCredit(montantacrediter : Double) : Double = {
      val transactionfees = (montantacrediter * feesRate) /100
      availableMoney = availableMoney + montantacrediter - transactionfees
      availableMoney
    }

    def afficheVerif(): Unit = {
      println("Customer id: " + customerId + ", Pin Code: " + codePin + ", Available amount: " + availableMoney)
    }
  }

  def lireEwallet(): ArrayBuffer[Ewallet] = {
    val file = "ewallet.txt"
    if (!new File(file).exists) {
      new File(file).createNewFile()
      ArrayBuffer[Ewallet]()
    } else {
      val ews = ArrayBuffer[Ewallet]()
      val bufferedSource = Source.fromFile(file)
      for (line <- bufferedSource.getLines) {
        val e = line.split(",")
        val ewallet = new Ewallet(e(0), e(2).toDouble, e(1).toInt)
        ews.append(ewallet)
      }
      bufferedSource.close()
      ews
    }
  }

  def sauverEwallet(listeEwallet : ArrayBuffer[Ewallet]) : Unit = {
    val writer = new PrintWriter(new File("ewallet.txt"))
    for (ewallet <- listeEwallet) {
      writer.write(ewallet.customerId + "," + ewallet.codePin + "," + ewallet.availableMoney + "\n")
    }
    writer.close()
  }

  def main(args: Array[String]): Unit = {
    val ewallets = lireEwallet()

    var choice = readLine("Menu\n1. Create an e-wallet\n2. Access your e-wallet\n3. Quit the program\nEnter your choice: ").toInt
    while (choice != 3) {
      if (choice == 1) {
        val customerId = readLine("  Enter your customer code: ")
        var isExists: Boolean = false
        var i = 0
        while (i < ewallets.length && !isExists) {
          if (ewallets(i).customerId == customerId) {
            isExists = true
          }
          i += 1
        }
        if (isExists) {
          println("  Ewallet creation denied. A customer can only have one ewallet at a time")
        } else {
          val initialAmount = readLine("  Enter initial amount: ").toDouble
          val newWallet = new Ewallet(customerId, initialAmount)
          ewallets.append(newWallet)
          newWallet.afficheVerif()
        }
      } else {
        val customerId = readLine("  Enter your customer code: ")
        val pinCode = readLine("  Enter your pin code: ").toInt
        var matchedWallet: Ewallet = null
        var customerExists: Boolean = false
        var i = 0
        while (matchedWallet == null && i < ewallets.length) {
          val ewallet = ewallets(i)
          if (ewallet.customerId == customerId) {
            customerExists = true
          }
          if (ewallet.verifieClient(customerId, pinCode)) {
            matchedWallet = ewallet
          }
          i += 1
        }
        if (matchedWallet == null) {
          if (customerExists) {
            println("  Operation is cancelled due to incorrect pin code")
          } else {
            println("  Operation is cancelled due to account doesn't exist")
          }
        } else {
          val subChoice = readLine("    Menu\n    1. Credit\n    2. Debit\n    Enter your choice: ").toInt
          if (subChoice == 1) {
            val amount = readLine("    Enter amount to be credited: ").toDouble
            println("    Avialable account balance: " + matchedWallet.operationCredit(amount))
          } else {
           val amount = readLine("    Enter amount to be debited: ").toDouble
            val debAmount =  matchedWallet.operationDebit(amount)
            if (debAmount.isNaN) {
              println("    Transaction failed due to insufficient balance")
            }
            println("    Avialable account balance: " + matchedWallet.availableMoney)
          }
        }
      }
      choice = readLine("Menu\n1. Create an e-wallet\n2. Access your e-wallet\n3. Quit the program\nEnter your choice: ").toInt
    }
    for (ewallet <- ewallets) {
      ewallet.afficheVerif()
    }
    sauverEwallet(ewallets)
  }
}
