package fsm;

import java.util.function.Function;

/**
 * Simulator => new Machine , new Inputs
 * <p>
 * CandyMachine
 * inputs -> state transition, candies, coins
 * State
 * next State, action
 * <p>
 * locked, candies >0
 * Coin Input => (candies --, coins ++) next State(
 * <p>
 * Created by hyewon on 2016. 1. 9..
 */
public class Simulator {

  public static void main(String[] args) {

    System.out.println(new CandyMachine(true, 10, 0)
        .input(Input.Coin)
        .input(Input.Coin)
        .input(Input.Turn));
  }
}


enum Input {
  Coin(m -> {
    if (m.candyAcceptable()) {
      return new CandyMachine(false, m.getCandies(), m.getCoins());
    } else {
      return m;
    }
  }),
  Turn(m -> {
    if (m.turnAvailable()) {
      return new CandyMachine(true, m.getCandies() - 1, m.getCoins() + 1);
    } else {
      return m;
    }
  });


  private Function<CandyMachine, CandyMachine> action;

  Input(Function<CandyMachine, CandyMachine> action) {
    this.action = action;
  }

  public Function<CandyMachine, CandyMachine> getAction() {
    return action;
  }
}


class CandyMachine {
  private boolean locked;
  private int candies;
  private int coins;

  private boolean remainCandy() {
    return candies > 0;
  }

  public boolean candyAcceptable() {
    return locked && remainCandy();
  }

  public boolean turnAvailable() {
    return !locked && remainCandy();
  }

  public CandyMachine(boolean locked, int candies, int coins) {
    this.locked = locked;
    this.candies = candies;
    this.coins = coins;
  }

  public CandyMachine input(Input input) {
    return input.getAction().apply(this);
  }


  public int getCandies() {
    return candies;
  }

  public int getCoins() {
    return coins;
  }

  @Override
  public String toString() {
    return "CandyMachine{" +
        "locked=" + locked +
        ", candies=" + candies +
        ", coins=" + coins +
        '}';
  }
}
