package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*

object InputButtonsComponent:
  case class ChoiceElement[+A](
    value: A,
    text: String,
    front: Option[TextColor.ANSI],
    back: Option[TextColor.ANSI],
  )
  object ChoiceElement:
    def of[A <: Singleton](
      value: A,
      text: String,
      front: Option[TextColor.ANSI] = None,
      back: Option[TextColor.ANSI] = None,
    ): ChoiceElement[A] =
      ChoiceElement(value, text, front = front, back = back)

    extension [A](choice: ChoiceElement[A])
      def onClick[R](requires: Requires[R]): Choice[A, R] = Choice(choice, requires)
      def onClickNext[B, R](select: Select[B, R]): Choice[A, R] = Choice(choice, Requires.NextChoice(select))
      def onClickReady[R](build: A => R): Choice[A, R] = Choice(choice, Requires.Ready(build(choice.value)))
      private[InputButtonsComponent] def label: Label =
        val base = Label(choice.text)
        choice.front.foreach(base.setForegroundColor)
        choice.back.foreach(base.setBackgroundColor)
        base

  case class Choice[+A, +R](element: ChoiceElement[A], onClick: Requires[R])

  case class SelectPrefix(text: Option[String] = None)
  object SelectPrefix:
    extension (prefix: SelectPrefix)
      def withOptions[A, R](options: Seq[Choice[A, R]]): Select[A, R] = Select(prefix, options)
      private[InputButtonsComponent] def label: Option[Label] =
        prefix.text.map(Label(_))

  sealed trait Requires[+R]
  object Requires:
    case class NextChoice[A, R](select: Select[A, R]) extends Requires[R]
    case class Ready[R](result: R) extends Requires[R]

  case class Accumulated(passed: Seq[(SelectPrefix, ChoiceElement[?])])
  object Accumulated:
    extension (acc: Accumulated)
      def add(select: SelectPrefix, choice: ChoiceElement[?]): Accumulated =
        Accumulated(acc.passed :+ (select -> choice))
      private[InputButtonsComponent] def labels: Seq[Label] =
        acc.passed.flatMap: (select, choice) =>
          select.label.toSeq :+ choice.label

  case class Select[A, +R](prefix: SelectPrefix, options: Seq[Choice[A, R]])
  case class InputState[R](undo: Option[InputState[R]], acc: Accumulated, next: Requires[R])

  trait Submit[R]:
    def result(value: R): Unit

  def render[R](initial: InputState[R], submit: Submit[R]): Component =
    DynamicComponent
      .selfUpdatableOnly[InputState[R]](initial) { (input, update) =>
        Panel(GridLayout(2))
          .withAll(
            Label("Command:"),
            command(input),
            EmptySpace(),
            buttons(input, update, submit),
          )
      }
      .withBorder(Borders.singleLine("Input"))

  private def command[R](input: InputState[R]): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withSeq:
      input.acc.labels ++ (input.next match {
        case Requires.NextChoice(select) => select.prefix.label.toSeq :+ Label("___")
        case Requires.Ready(_) => List.empty
      })

  private def buttons[R](
    input: InputState[R],
    components: DynamicComponent.Update[InputState[R]],
    submit: Submit[R],
  ): Panel =
    val undoButtonComponent: Component =
      input.undo.fold(Panel()): previous =>
        undoButton(components.update(previous))
    input.next match
      case Requires.Ready(result) =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          submitButton(submit.result(result)),
          undoButtonComponent,
        )
      case Requires.NextChoice(select) =>
        def mainButtons: Panel =
          val totalChoices = select.options.size
          val bestGridColumns = math.ceil(math.sqrt(totalChoices)).toInt
          Panel(GridLayout(bestGridColumns)).withSeq:
            select.options.map: choice =>
              button(choice.element.text) {
                components.update {
                  InputState(
                    undo = Some(input),
                    acc = input.acc.add(select.prefix, choice.element),
                    next = choice.onClick,
                  )
                }
              }
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          mainButtons,
          undoButtonComponent,
        )

  private def undoButton(onClick: => Unit): Button =
    button("| undo |")(onClick)

  private def submitButton(onClick: => Unit): Button =
    button("| submit |")(onClick)

  private def button(name: String)(onClick: => Unit): Button =
    Button(name, () => onClick)
