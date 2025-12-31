package com.zkerriga.buckshot.tui

object domain {
  enum Letter:
    case A, B

  enum ALevel:
    case A1, A2

  enum BLevel:
    case B1, B2, B3

  enum B2Stage:
    case B21, B22

  // UI domain

  type AMessage = (Letter.A.type, ALevel)
  type B1Message = (Letter.B.type, BLevel.B1.type)
  type B2Message = (Letter.B.type, BLevel.B2.type, B2Stage)
  type B3Message = (Letter.B.type, BLevel.B3.type)
  type BMessage = B1Message | B2Message | B3Message

  type Result = AMessage | BMessage

  import IdeaComponent.*

  object A1Choice extends Choice[ALevel.A1.type, Letter.A.type, Result] {
    override def label: Label = Label("A1")
    override def value: ALevel.A1.type = ALevel.A1
    override def next(acc: Accumulated[Letter.A.type]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value, value))
  }

  object A2Choice extends Choice[ALevel.A2.type, Letter.A.type, Result] {
    override def label: Label = Label("A2")
    override def value: ALevel.A2.type = ALevel.A2
    override def next(acc: Accumulated[Letter.A.type]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value, value))
  }

  object ALevelSelect extends Select[ALevel, Letter.A.type, Result] {
    override def options: List[Choice[ALevel, Letter.A.type, Result]] =
      List(
        A1Choice,
        A2Choice,
      )
  }

  object AChoice extends Choice[Letter.A.type, Unit, Result] {
    override def label: Label = Label("A")
    override def value: Letter.A.type = Letter.A
    override def next(acc: Accumulated[Unit]): ChoiceContinuation[Result] =
      ChoiceContinuation.NextChoice(Accumulated(value, acc.rendered :+ label), ALevelSelect)
  }

  object B1Choice extends Choice[BLevel.B1.type, Letter.B.type, Result] {
    override def label: Label = Label("B1")
    override def value: BLevel.B1.type = BLevel.B1
    override def next(acc: Accumulated[Letter.B.type]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value, value))
  }

  object B21Choice extends Choice[B2Stage.B21.type, (Letter.B.type, BLevel.B2.type), Result] {
    override def label: Label = Label("B21")
    override def value: B2Stage.B21.type = B2Stage.B21
    override def next(acc: Accumulated[(Letter.B.type, BLevel.B2.type)]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value._1, acc.value._2, value))
  }

  object B22Choice extends Choice[B2Stage.B22.type, (Letter.B.type, BLevel.B2.type), Result] {
    override def label: Label = Label("B22")
    override def value: B2Stage.B22.type = B2Stage.B22
    override def next(acc: Accumulated[(Letter.B.type, BLevel.B2.type)]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value._1, acc.value._2, value))
  }

  object B2StageSelect extends Select[B2Stage, (Letter.B.type, BLevel.B2.type), Result] {
    override def options: List[Choice[B2Stage, (Letter.B.type, BLevel.B2.type), Result]] =
      List(B21Choice, B22Choice)
  }

  object B2Choice extends Choice[BLevel.B2.type, Letter.B.type, Result] {
    override def label: Label = Label("B2")
    override def value: BLevel.B2.type = BLevel.B2
    override def next(acc: Accumulated[Letter.B.type]): ChoiceContinuation[Result] =
      ChoiceContinuation.NextChoice[B2Stage, (Letter.B.type, BLevel.B2.type), Result](
        Accumulated((acc.value, value), acc.rendered :+ label),
        B2StageSelect,
      )
  }

  object B3Choice extends Choice[BLevel.B3.type, Letter.B.type, Result] {
    override def label: Label = Label("B3")
    override def value: BLevel.B3.type = BLevel.B3
    override def next(acc: Accumulated[Letter.B.type]): ChoiceContinuation[Result] =
      ChoiceContinuation.Ready((acc.value, value))
  }

  object BLevelSelect extends Select[BLevel, Letter.B.type, Result] {
    override def options: List[Choice[BLevel, Letter.B.type, Result]] =
      List(
        B1Choice,
        B2Choice,
        B3Choice,
      )
  }

  object BChoice extends Choice[Letter.B.type, Unit, Result] {
    override def label: Label = Label("B")
    override def value: Letter.B.type = Letter.B
    override def next(acc: Accumulated[Unit]): ChoiceContinuation[Result] =
      ChoiceContinuation.NextChoice[BLevel, Letter.B.type, Result](
        Accumulated(value, acc.rendered :+ label),
        BLevelSelect,
      )
  }

  object StartSelect extends Select[Letter, Unit, Result] {
    override def options: List[Choice[Letter, Unit, Result]] =
      List(AChoice, BChoice)
  }

  def initial(someGameData: Boolean): ChoiceContinuation[Result] =
    if someGameData then BChoice.next(Accumulated((), List(Label("Start with B"))))
    else ChoiceContinuation.NextChoice(Accumulated((), List()), StartSelect)
}

object IdeaComponent {
  case class Label(text: String)

  enum Button[R]:
    case Submit(submit: R)
    case Undo(setPrevious: InputState[R])
    case Next(label: Label, setNext: InputState[R])

  sealed trait ChoiceContinuation[R]
  object ChoiceContinuation {
    case class Ready[R](result: R) extends ChoiceContinuation[R]
    case class NextChoice[A, Acc, R](acc: Accumulated[Acc], select: Select[A, Acc, R]) extends ChoiceContinuation[R]
  }

  trait Choice[+A, Acc, R]:
    def label: Label
    def value: A
    def next(acc: Accumulated[Acc]): ChoiceContinuation[R]

  trait Select[A, Acc, R]:
    def options: List[Choice[A, Acc, R]]

  case class Accumulated[Acc](value: Acc, rendered: List[Label])

  /** @tparam R
    *   final result
    */
  sealed trait InputState[R]
  object InputState {
    case class Choose[A, Acc, R](undo: Option[InputState[R]], acc: Accumulated[Acc], select: Select[A, Acc, R])
        extends InputState[R]

    case class ReadyToSubmit[R](undo: InputState[R], value: R) extends InputState[R]
  }

  /** should render a line with accumulated labels and also suggest if the next label can be added
    */
  def renderAccumulatedCommand[R](state: InputState[R]): List[Label] =
    state match {
      case InputState.ReadyToSubmit(_, _) => List()
      case InputState.Choose(acc = acc) => acc.rendered :+ Label("___")
    }

  def renderButtons[R](state: InputState[R]): List[Button[R]] =
    state match {
      case InputState.ReadyToSubmit(undo = undo, value = value) =>
        List(
          Button.Submit(value),
          Button.Undo(undo),
        )
      case InputState.Choose(undo, acc, select) =>
        List(
          select.options.map { choice =>
            Button.Next(
              choice.label,
              choice.next(acc) match {
                case ChoiceContinuation.Ready(result) => InputState.ReadyToSubmit(state, result)
                case ChoiceContinuation.NextChoice(acc, select) => InputState.Choose(Some(state), acc, select)
              },
            )
          },
          undo.map(Button.Undo[R]).toList,
        ).flatten
    }
}
