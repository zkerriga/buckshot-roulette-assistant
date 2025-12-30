package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*

trait DynamicComponent[A] extends DynamicComponent.Update[A]:
  val component: Component

object DynamicComponent:
  trait Update[A]:
    def update(updated: A): Unit

  def of[A](initial: A, render: A => Component): DynamicComponent[A] =
    new:
      val component: Panel = Panel().withAll(render(initial))
      def update(updated: A): Unit =
        component.removeAllComponents()
        component.addComponent(render(updated))
        component.invalidate()
