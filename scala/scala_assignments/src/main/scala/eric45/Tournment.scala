package eric45

/*
Assume two teams, A and B are playing in a tournament. The first team that wins k games wins. Assume the probability that either team wins any one game is 0.5. Find a recursive implementation of:

probability(n, m) = the probability A wins the tournament assuming A must win n more games and B must win m more games.

Notes

1. For example, the World Series is a 7-game tournament. The first team to win 4 games wins the tournament. If the teams are evenly matched, then there's a 50% chance either team will win any particular game. If team B wins the first 3 games of the tournament, then the probability that A will win the World Series is probability(4, 1).

2. Observe that:

probability(0, m) = 1 // A won
probability(n, 0) = 0 // A lost

3. If A wins the next game, then he only needs to win n – 1 more games, but B still has to win m games.

4. If A looses the next game, then he still needs to win n games, but now B only needs to win m – 1 games.

5. Throw an exception if n or m is negative.

*/
object Tournment extends App {

  // prob A wins if A needs n wins and B needs m
  def probability(n: Int, m: Int): Double = {
    if (n < 0 || m < 0)
      throw new Exception("n and m should not be negative")
    if (n == 0) {
      1.0
    } else if (m == 0) {
      0.0
    } else {
      0.5 * (probability(n-1, m) + probability(n, m-1))
    }
  }

  // Team A sweeps the World Series
  for(i <- 4 to 0 by -1)
    println("probability A wins = " + probability(i, 4))
}

/*
Output

probability A wins = 0.5
probability A wins = 0.65625
probability A wins = 0.8125
probability A wins = 0.9375
probability A wins = 1.0

*/