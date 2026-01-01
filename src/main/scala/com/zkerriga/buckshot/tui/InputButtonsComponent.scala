package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*

object InputButtonsComponent:
  case class ChoiceElement[A](value: A, text: String, label: () => Label)
  object ChoiceElement:
    def of[A <: Singleton](value: A, text: String): ChoiceElement[A] =
      ChoiceElement(value, text, () => Label(text))

    extension [A](element: ChoiceElement[A]) {
      def onClick[R](f: () => Requires[R]): Choice[A, R] =
        Choice(element, f)
      def onClickNext[B, R](select: Select[B, R]): Choice[A, R] =
        Choice(element, () => Requires.next(select))
      def onClickReady[R](build: A => R): Choice[A, R] =
        Choice(element, () => Requires.Ready(build(element.value)))
    }

  sealed trait Requires[+R]
  object Requires:
    case class NextChoice[A, R](select: Select[A, R]) extends Requires[R]
    case class Ready[R](result: R) extends Requires[R]

    def next[A, R](select: Select[A, R]): NextChoice[A, R] = NextChoice(select)

  case class Choice[+A, +R](
    value: A,
    text: String,
    label: () => Label,
    onClick: () => Requires[R],
  )
  object Choice:
    def apply[A, R](element: ChoiceElement[A], onClick: () => Requires[R]): Choice[A, R] =
      Choice(element.value, element.text, element.label, onClick)

  case class Select[A, +R](
    description: Option[Label],
    options: Seq[Choice[A, R]],
  )

  case class Accumulated(labels: () => Seq[Label])

  sealed trait InputState[R]
  object InputState:
    case class ReadyToSubmit[A, R](undo: InputState.Choose[A, R], acc: Accumulated, result: R) extends InputState[R]
    case class Choose[A, R](undo: Option[InputState[R]], acc: Accumulated, select: Select[A, R]) extends InputState[R]

  trait Submit[R]:
    def result(value: R): Unit

  def render[R](initial: InputState.Choose[?, R], submit: Submit[R]): Component =
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
      input match
        case InputState.ReadyToSubmit(acc = accumulated) => accumulated.labels()
        case InputState.Choose(acc = accumulated) => accumulated.labels() :+ Label("___")

  private def buttons[R](
    input: InputState[R],
    components: DynamicComponent.Update[InputState[R]],
    submit: Submit[R],
  ): Panel =
    input match
      case InputState.ReadyToSubmit(undo, _, result) =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          submitButton(submit.result(result)),
          undoButton(components.update(undo)),
        )
      case input @ InputState.Choose(undo, acc, select) =>
        def mainButtons: Panel =
          val totalChoices = select.options.size
          val bestGridColumns = math.ceil(math.sqrt(totalChoices)).toInt
          Panel(GridLayout(bestGridColumns)).withSeq:
            select.options.map: choice =>
              button(choice.text) {
                components.update {
                  choice.onClick() match
                    case Requires.NextChoice(select) =>
                      InputState.Choose(
                        undo = Some(input),
                        acc = Accumulated(
                          labels = () => acc.labels() :+ choice.label() :++ select.description,
                        ),
                        select = select,
                      )
                    case Requires.Ready(result) =>
                      InputState.ReadyToSubmit(
                        undo = input,
                        acc = Accumulated(labels = () => acc.labels() :+ choice.label()),
                        result = result,
                      )
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
