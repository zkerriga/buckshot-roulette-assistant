package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.{Component, Panel}

package object tui:
  extension (panel: Panel)
    def withAll(components: Component*): Panel =
      components.foreach(panel.addComponent)
      panel

    def withSeq(components: Seq[Component]): Panel = withAll(components*)
