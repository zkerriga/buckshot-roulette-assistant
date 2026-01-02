package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.journal.AppLog.Logging

import scala.jdk.CollectionConverters.*

trait DynamicComponent[A] extends DynamicComponent.Update[A]:
  val component: Component

object DynamicComponent extends Logging:
  trait Update[A]:
    def update(updated: A): Unit

  private given [A]: Conversion[java.util.Collection[A], List[A]] = _.asScala.toList

  private def searchFocused(components: List[Component]): Option[Interactable] =
    components match
      case Nil => None
      case head :: next =>
        val headIsFocused = head match
          case interactable: Interactable => Option.when(interactable.isFocused)(interactable)
          case panel: Panel => searchFocused(panel.getChildren)
          case border: Border => searchFocused(List(border.getComponent))
          case _ => None
        headIsFocused.orElse(searchFocused(next))

  private def searchFirstFocusable(components: List[Component]): Option[Interactable] =
    components match
      case Nil => None
      case head :: next =>
        val headIsFocusable = head match
          case interactable: Interactable => Option.when(interactable.isFocusable)(interactable)
          case panel: Panel => searchFirstFocusable(panel.getChildren)
          case border: Border => searchFirstFocusable(List(border.getComponent))
          case _ => None
        headIsFocusable.orElse(searchFirstFocusable(next))

  def selfUpdatable[A](initial: A)(render: (A, Update[A]) => Component): DynamicComponent[A] =
    new:
      val component: Panel = Panel().withAll(render(initial, this))
      def update(updated: A): Unit =
        val focused = searchFocused(component.getChildren)
        log.trace(s"update called with $updated $focused")
        component.removeAllComponents()
        component.addComponent(render(updated, this))
        component.invalidate()
        for
          _ <- focused
          focusable <- searchFirstFocusable(component.getChildren)
        yield
          log.trace(s"update found a new interactable to take focus $focusable")
          focusable.takeFocus()

  def updatable[A](initial: A)(render: A => Component): DynamicComponent[A] =
    selfUpdatable(initial)((a, _) => render(a))

  def selfUpdatableOnly[A](initial: A)(render: (A, Update[A]) => Component): Component =
    selfUpdatable(initial)(render).component
