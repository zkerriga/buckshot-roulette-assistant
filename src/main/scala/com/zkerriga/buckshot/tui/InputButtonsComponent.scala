package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*

object InputButtonsComponent:
  case class ChoiceElement[A](value: A, text: String, label: Label)
  object ChoiceElement:
    def of[A <: Singleton](value: A, text: String): ChoiceElement[A] =
      ChoiceElement(value, text, Label(text))

    final class PartialChoice[A, Acc] private[ChoiceElement] (element: ChoiceElement[A]):
      def onClickFinalizeTo[R](using builder: AccBuilder[Acc, A, R]): Choice[A, Acc, R] =
        Choice(element, acc => Requires.Ready(builder.build(acc, element.value)))

      def onClickContinueTo[B, Acc2, R](
        select: Select[B, Acc2, R],
      )(using builder: AccBuilder[Acc, A, Acc2]): Choice[A, Acc, R] =
        Choice(element, acc => Requires.NextChoice(builder.build(acc, element.value), select))

    extension [A](element: ChoiceElement[A]) def whenState[Acc]: PartialChoice[A, Acc] = PartialChoice(element)

  trait AccBuilder[Before, A, After]:
    def build(before: Before, value: A): After
  object AccBuilder:
    given [A]: AccBuilder[Unit, A, A] = (_, value) => value

  sealed trait Requires[+R]
  object Requires:
    case class NextChoice[A, Acc, R](acc: Acc, select: Select[A, Acc, R]) extends Requires[R]
    case class Ready[R](result: R) extends Requires[R]

  case class Choice[A, Acc, +R](
    element: ChoiceElement[A],
    onClick: Acc => Requires[R],
  )
  case class Select[A, Acc, +R](
    description: Option[Label],
    options: Seq[Choice[A, Acc, R]],
  )

  case class Accumulated[Acc](value: Acc, labels: Seq[Label])

  sealed trait InputState[R]
  object InputState:
    case class ReadyToSubmit[R](undo: InputState[R], result: R) extends InputState[R]
    case class Choose[A, Acc, R](undo: Option[InputState[R]], acc: Accumulated[Acc], select: Select[A, Acc, R])
        extends InputState[R]

  trait Submit[R]:
    def result(value: R): Unit

  def render[R](initial: InputState.Choose[?, ?, R], submit: Submit[R]): Component =
    DynamicComponent.selfUpdatableOnly[InputState[R]](initial) { (input, update) =>
      Panel(GridLayout(2))
        .withAll(
          Label("Command:"),
          command(input),
          EmptySpace(),
          buttons(input, update, submit),
        )
        .withBorder(Borders.singleLine("Input"))
    }

  private def command[R](input: InputState[R]): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withSeq:
      input match
        case InputState.ReadyToSubmit(_, _) => Seq.empty
        case InputState.Choose(acc = accumulated) =>
          accumulated.labels :+ Label("___")

  private def buttons[R](
    input: InputState[R],
    components: DynamicComponent.Update[InputState[R]],
    submit: Submit[R],
  ): Panel =
    input match
      case InputState.ReadyToSubmit(undo, result) =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          submitButton(submit.result(result)),
          undoButton(components.update(undo)),
        )
      case InputState.Choose(undo, acc, select) =>
        def mainButtons: Panel =
          val totalChoices = select.options.size
          val bestGridColumns = math.ceil(math.sqrt(totalChoices)).toInt
          Panel(GridLayout(bestGridColumns)).withSeq:
            select.options.map: choice =>
              button(choice.element.text) {
                components.update {
                  choice.onClick(acc.value) match
                    case Requires.NextChoice(accValue, select) =>
                      InputState.Choose(
                        undo = Some(input),
                        acc = Accumulated(
                          value = accValue,
                          labels = acc.labels :+ choice.element.label :++ select.description,
                        ),
                        select = select,
                      )
                    case Requires.Ready(result) =>
                      InputState.ReadyToSubmit(undo = input, result = result)
                }
              }
        undo match
          case Some(undoState) =>
            Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
              mainButtons,
              undoButton(components.update(undoState)),
            )
          case None => mainButtons

  private def undoButton(onClick: => Unit): Button =
    button("| undo |")(onClick)

  private def submitButton(onClick: => Unit): Button =
    button("| submit |")(onClick)

  private def button(name: String)(onClick: => Unit): Button =
    Button(name, () => onClick)
