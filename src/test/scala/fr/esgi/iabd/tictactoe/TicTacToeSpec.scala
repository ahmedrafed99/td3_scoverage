package fr.esgi.iabd.tictactoe

import java.io.{ByteArrayOutputStream, StringReader}

import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.matchers.should.Matchers

class TicTacToeSpec extends AnyFlatSpec with Matchers {
  "play" should "add X for player 1" in {
    // Given
    val grid = Vector(Vector(".", ".", "."))
    val (x, y) = (0, 0)
    val player = 1

    // When
    val newGrid = TicTacToe.play(grid, x, y, player)

    // Then
    newGrid shouldBe Vector(Vector("X", ".", "."))
  }

  "vertialVictory" should "validate vertical victory" in {
    // Given
    val grid = Vector(
      Vector(".", "X", "O"),
      Vector("O", "O", "O"),
      Vector("O", "X", "O")
    )

    // Then
    TicTacToe.verticalVictory(grid, 2) shouldBe "Player 0"
  }

  "horizontalVictory" should "validate horizontal victory" in {
    // Given
    val grid = Vector(
      Vector(".", "X", "O"),
      Vector("X", "X", "X"),
      Vector("O", "X", "O")
    )

    // Then
    TicTacToe.horizontalVictory(grid, 0) shouldBe "Nobody"
    TicTacToe.horizontalVictory(grid, 1) shouldBe "Player 1"
    TicTacToe.horizontalVictory(grid, 2) shouldBe "Nobody"
  }

  "victory" should "find victory in any situation" in {
    TicTacToe.victory(
      Vector(
        Vector("O", "X", "O"),
        Vector("X", "X", "."),
        Vector("O", "X", "O")
      )
    ) shouldBe "Player 1"

    TicTacToe.victory(
      Vector(
        Vector("O", "X", "O"),
        Vector("X", "X", "O"),
        Vector("O", "X", "O")
      )
    ) shouldBe "Player 1"

    TicTacToe.victory(
      Vector(
        Vector("X", "X", "O"),
        Vector("O", "O", "O"),
        Vector("X", "X", ".")
      )
    ) shouldBe "Player 0"

    TicTacToe.victory(
      Vector(
        Vector("O", "O", "O"),
        Vector("X", ".", "."),
        Vector("X", "X", "X")
      )
    ) shouldBe "Player 0"
  }
}
