package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.WindowBasedTextGUI
import com.googlecode.lanterna.gui2.dialogs.{MessageDialog, MessageDialogButton}
import com.zkerriga.buckshot.engine.events.ContinuableOutcome
import com.zkerriga.buckshot.game.events.outcome.Outcome.Reset
import com.zkerriga.buckshot.game.state.partitipant.Side

class Dialogs(gui: WindowBasedTextGUI):
  def showError(text: String): Unit =
    MessageDialog.showMessageDialog(gui, "Error", text, MessageDialogButton.OK)

  def showGameOutcome(winner: Side): Unit =
    MessageDialog.showMessageDialog(
      gui,
      "Game Over",
      s"$winner wins",
      MessageDialogButton.Close,
    )

  def showReset(reset: ContinuableOutcome.ResetDetails): Unit =
    MessageDialog.showMessageDialog(
      gui,
      "Shotgun Empty",
      s"Shotgun is empty, current state is $reset", // todo: think of game continuation
      MessageDialogButton.OK,
    )
