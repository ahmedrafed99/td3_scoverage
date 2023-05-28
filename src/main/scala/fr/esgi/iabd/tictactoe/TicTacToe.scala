package fr.esgi.iabd.tictactoe

import scala.annotation.tailrec

object TicTacToe {

  def main(args: Array[String]): Unit = {
    run(
      Vector(
        Vector(".", ".", "."),
        Vector(".", ".", "."),
        Vector(".", ".", ".")
      ),
      0
    )
  }

  @tailrec
  def run(grid: Vector[Vector[String]], player: Int): Unit = {
    display(grid)

    print(s"Player $player action: ")
    val input = scala.io.StdIn.readLine()

    println(input)
    val x = input.split(",")(0).toInt
    val y = input.split(",")(1).toInt

    val newGrid = play(grid, x, y, player)
    val newPlayer = (player + 1) % 2

    val winner = victory(newGrid)

    winner match {
      case "Player 0" | "Player 1" =>
        println(s"=====\nThe winner is $winner\n=====")
      case _ => run(newGrid, newPlayer)
    }
  }

  def display(grid: Vector[Vector[String]]): Unit = {
    grid.foreach { row =>
      row.foreach(print)
      println()
    }
  }

  def play(
      grid: Vector[Vector[String]],
      x: Int,
      y: Int,
      player: Int
  ): Vector[Vector[String]] = {
    val symbol = if (player == 0) "O" else "X"
    grid.updated(y, grid(y).updated(x, symbol))
  }

  def player(symbol: String): String =
    symbol match {
      case "O" => "Player 0"
      case _   => "Player 1"
    }

  def victory(grid: Vector[Vector[String]]): String = {
    if (horizontalVictory(grid, 0) != "Nobody") horizontalVictory(grid, 0)
    else if (horizontalVictory(grid, 1) != "Nobody") horizontalVictory(grid, 1)
    else if (horizontalVictory(grid, 2) != "Nobody") horizontalVictory(grid, 2)
    else if (verticalVictory(grid, 0) != "Nobody") verticalVictory(grid, 0)
    else if (verticalVictory(grid, 1) != "Nobody") verticalVictory(grid, 1)
    else if (verticalVictory(grid, 2) != "Nobody") verticalVictory(grid, 2)
    else if (
      grid(0)(0) != "." && grid(0)(0) == grid(1)(1) && grid(1)(1) == grid(2)(2)
    ) player(grid(0)(0))
    else if (
      grid(0)(2) != "." && grid(0)(2) == grid(1)(1) && grid(1)(1) == grid(2)(0)
    ) player(grid(0)(2))
    else "Nobody"
  }

  def verticalVictory(grid: Vector[Vector[String]], column: Int): String = {
    if (grid(0)(column) == ".") {
      "Nobody"
    } else {
      if (
        grid(0)(column) == grid(1)(column) && grid(1)(column) == grid(2)(column)
      ) {
        player(grid(0)(column))
      } else {
        "Nobody"
      }
    }
  }

  def horizontalVictory(grid: Vector[Vector[String]], row: Int): String = {
    if (grid(row)(0) == ".") {
      "Nobody"
    } else {
      if (grid(row)(0) == grid(row)(1) && grid(row)(1) == grid(row)(2)) {
        player(grid(row)(0))
      } else {
        "Nobody"
      }
    }
  }
}
