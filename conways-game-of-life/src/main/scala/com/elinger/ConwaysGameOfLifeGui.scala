package com.elinger

import com.elinger.ConwaysGameOfLife._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.swing.{BoxPanel, Font, Label, MainFrame, Orientation, SimpleSwingApplication, TextPane}

object ConwaysGameOfLifeGui extends SimpleSwingApplication {

  val label = new Label("")
  val textPane: TextPane = new TextPane {
    font = new Font("Monospaced", 0, 18)
  }

  def top: MainFrame = new MainFrame {
    title = "Conway's game of life"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += label
      contents += textPane
    }
    centerOnScreen()
    pack()
  }

  Future {
    val stateAsString = Source.fromResource("symmetrical-2.txt")
      .getLines().mkString("\n")
    textPane.text = stateAsString
    val state = convertToState(stateAsString)
    val numOfIterations = 30
    play(List(state), numOfIterations).reverse
      .zip(1 to numOfIterations)
      .foreach(t => {
        label.text    = s"Iteration: ${t._2}"
        textPane.text = ConwaysGameOfLife.toString(t._1)
        Thread.sleep(1000)
      })
    System.exit(0)
  }(ExecutionContext.global)
}
