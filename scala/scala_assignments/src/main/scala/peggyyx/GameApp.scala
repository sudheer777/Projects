package peggyyx

// package cw

import scala.io.StdIn

/**
 * This GameApp class is not needed for the coursework.
 * It is instead for explanatory purposes only.
 * Once you have some of your methods implemented (Various moves, collectCoin, etc.),
 * it can be ran to provide a short interactive demo for the initial game scenario.
 * This can be helpful for better understanding the game structure when tackling the harder methods
 */

object GameApp {
  def main(args: Array[String]): Unit = {
    var input:String=""
    var round:Int=1
    var game:Game=GameBuilder.initialiseGame1()
    while(!input.equals("q")){
      println("You are in round "+ round+" and your score is "+game.getScore())
      println("You could get up to "+game.maxScore()+" score");
      game.printField()
      input=StdIn.readLine("Enter next move (w,a,s,d,save):")
      if(input.equals("a"))
        game.moveLeft()
      else if(input.equals("d"))
        game.moveRight()
      else if(input.equals("w"))
        game.moveUp()
      else if(input.equals("s"))
        game.moveDown()
      else if(input.equals("save"))
        game.save()
      round += 1
    }
  }
}