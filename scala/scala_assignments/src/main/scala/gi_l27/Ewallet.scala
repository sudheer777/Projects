package gi_l27

import java.io.{File, FileWriter}

import scala.io.StdIn.readLine
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Ewallet {
  val random = scala.util.Random

  class Ewallet(val customerId: String, var availableBalance: Double, val pinCode: Int) {
    val overdraftFees: Int = 500
    val feesRate: Int = 3

    def this(customerId: String, initialBalance: Double) = {
      this(customerId, initialBalance, pinCode = random.nextInt(99900) + 100)
    }

    def verifieClient(idclient_verif : String, codepin_verif : Int) : Boolean = {
      customerId == idclient_verif && pinCode == codepin_verif
    }

    def afficheVerif(): Unit = {
      println("Customer id: " + customerId + ", Pin code: " + pinCode + ", Available balance: " + availableBalance)
    }

    def operationDebit(montantadebiter : Double) : Double = {
      val transFees = (montantadebiter * feesRate) /100
      val total = montantadebiter + transFees
      if (availableBalance + overdraftFees >= total) {
        availableBalance -= total
        availableBalance
      } else Double.NaN
    }

    def operationCredit(montantacrediter : Double) : Double = {
      val transFees = (montantacrediter * feesRate) /100
      availableBalance = availableBalance + montantacrediter - transFees
      availableBalance
    }
  }

  def lireEwallet(): ArrayBuffer[Ewallet] = {
    val file = new File("ewallet.txt")
    if (!file.exists) {
      file.createNewFile()
      ArrayBuffer[Ewallet]()
    } else {
      val tmpEwallets = ArrayBuffer[Ewallet]()
      val bufferedSource = Source.fromFile(file)
      for (line <- bufferedSource.getLines) {
        val col = line.split(",")
        val ewallet = new Ewallet(col(0), col(2).toDouble, col(1).toInt)
        tmpEwallets += ewallet
      }
      bufferedSource.close()
      tmpEwallets
    }
  }

  def sauverEwallet(listeEwallet : ArrayBuffer[Ewallet]) : Unit = {
    val writer = new FileWriter(new File("ewallet.txt"))
    for (ewallet <- listeEwallet) {
      val entry = ewallet.customerId + "," + ewallet.pinCode + "," + ewallet.availableBalance + "\n"
      writer.write(entry)
    }
    writer.close()
  }

  def main(args: Array[String]): Unit = {
    val ewallets = lireEwallet()

    var choice = 1
    while (choice != 3) {
      choice = readLine("Menu\n1. Create an e-wallet\n2. Access your e-wallet\n3. Quit the program\nEnter your choice: ").toInt
      if (choice == 1) {
        val customerId = readLine("  Enter your customer code: ")
        var customerExists: Boolean = false
        var ind = 0
        while (ind < ewallets.length && !customerExists) {
          if (ewallets(ind).customerId == customerId) {
            customerExists = true
          }
          ind += 1
        }
        if (customerExists) {
          println("  Ewallet creation failed. A customer can not have more than one ewallet")
        } else {
          val initialAmount = readLine("  Enter initial amount: ").toDouble
          val newWallet = new Ewallet(customerId, initialAmount)
          ewallets += newWallet
          newWallet.afficheVerif()
        }
      } else if (choice == 2) {
        val customerId = readLine("  Enter your customer code: ")
        val pinCode = readLine("  Enter your pincode: ").toInt
        var matchedWallet: Ewallet = null
        var customerExists: Boolean = false
        var ind = 0
        while (matchedWallet == null && ind < ewallets.length) {
          val ewallet = ewallets(ind)
          if (ewallet.customerId == customerId) {
            customerExists = true
          }
          if (ewallet.verifieClient(customerId, pinCode)) {
            matchedWallet = ewallet
          }
          ind += 1
        }
        if (matchedWallet == null) {
          if (customerExists) {
            println("  Could not complete the operation. Incorrect pin code")
          } else {
            println("  Could not complete the operation. customer id does not exist")
          }
        } else {
          val subChoice = readLine("    Menu\n    1. Credit\n    2. Debit\n    Enter your choice: ").toInt
          if (subChoice == 1) {
            val amount = readLine("    Enter amount to be credited: ").toDouble
            println("    Transaction successful!!!\n    Avialable ewallet balance: " + matchedWallet.operationCredit(amount))
          } else {
            val amount = readLine("    Enter amount to be debited: ").toDouble
            val debAmount =  matchedWallet.operationDebit(amount)
            if (debAmount.isNaN) {
              println("    Transaction unsuccessful!! Insufficient balance")
            } else {
              println("    Transaction successful!!!")
            }
            println("    Avialable ewallet balance: " + matchedWallet.availableBalance)
          }
        }
      }
    }

    ewallets.foreach(_.afficheVerif())

    sauverEwallet(ewallets)
  }
}
