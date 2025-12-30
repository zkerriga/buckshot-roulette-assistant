package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.accessors.Opposition
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.state.partitipant.Side

object InputComponent:
  enum Input:
    case ChooseEvent
    case ShotSet(input: InputTarget, undoDisabled: Boolean)
    case UsedSet(input: InputItem)

  enum InputTarget:
    case ChooseTarget
    case TargetSet(target: Side, input: InputShell)

  enum InputShell:
    case ChooseShell
    case ShellSet(shell: Shell) // end of input

  enum InputItem:
    case ChooseItem
    case SimpleItemSet(item: Handcuffs.type | Cigarettes.type | Saw.type | Inverter.type) // end of input
    case MagnifyingGlassSet(item: MagnifyingGlass.type, input: Option[InputShell])
    case BeerSet(item: Beer.type, input: InputShell)
    case BurnerPhoneSet(item: BurnerPhone.type, input: Option[InputSeqNr])
    case MedsSet(item: Meds.type, input: InputMedsQuality)
    case AdrenalineSet(input: InputItemToSteal)

  enum InputItemToSteal:
    case ChooseItem
    case SimpleItemSet(item: Handcuffs.type | Cigarettes.type | Saw.type | Inverter.type) // end of input
    case MagnifyingGlassSet(item: MagnifyingGlass.type, input: Option[InputShell])
    case BeerSet(item: Beer.type, input: InputShell)
    case BurnerPhoneSet(item: BurnerPhone.type, input: Option[InputSeqNr])
    case MedsSet(item: Meds.type, input: InputMedsQuality)

  enum InputSeqNr:
    case ChooseSeqNr
    case SeqNrSet(seqNr: SeqNr, input: InputShell)

  enum InputMedsQuality:
    case ChooseQuality
    case QualitySet(good: Boolean) // end of input

  object Input:
    def from(game: TableState)(using Opposition[TableState]): Input =
      if game.actor.items.empty then Input.ShotSet(InputTarget.ChooseTarget, undoDisabled = true)
      else Input.ChooseEvent

  def render(game: GameState): Component = ???

  private def r(game: TableState, state: Input): Component = {
    given Opposition[TableState] = game.turn match
      case Player => Opposition.of(actorIs = _.player, opponentIs = _.dealer)
      case Dealer => Opposition.of(actorIs = _.dealer, opponentIs = _.player)

    val update = new DynamicComponent.Update[Input] {
      def update(updated: Input): Unit = () // todo
    }
    val submit = new Submit {
      def event(event: Engine.Event): Unit = () // todo
    }

    Panel(GridLayout(2)).withAll(
      Label("Command"),
      command(game, state),
      EmptySpace(),
      buttons(game, state, update, submit),
    )
  }

  trait Submit:
    def event(event: Engine.Event): Unit

  private def buttons(
    game: TableState,
    input: Input,
    components: DynamicComponent.Update[Input],
    submit: Submit,
  )(using Opposition[TableState]): Panel =
    input match {
      case Input.ChooseEvent =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          Button(
            "Shot",
            () => components.update(Input.ShotSet(InputTarget.ChooseTarget, undoDisabled = false)),
          ),
          Button(
            "Used",
            () => components.update(Input.UsedSet(InputItem.ChooseItem)),
          ),
        )

      case shotSet @ Input.ShotSet(input, undoDisabled) =>
        input match {
          case InputTarget.ChooseTarget =>
            Panel(LinearLayout(Direction.HORIZONTAL)).withSeq:
              Seq(
                Button(
                  "Dealer",
                  () => components.update(shotSet.copy(input = InputTarget.TargetSet(Dealer, InputShell.ChooseShell))),
                ).some,
                Button(
                  "Player",
                  () => components.update(shotSet.copy(input = InputTarget.TargetSet(Player, InputShell.ChooseShell))),
                ).some,
                Option.unless(undoDisabled)(undoButton(() => components.update(Input.ChooseEvent))),
              ).flatten

          case targetSet @ InputTarget.TargetSet(target, input) =>
            input match {
              case InputShell.ChooseShell =>
                Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                  Button(
                    "Live",
                    () => components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ShellSet(Live)))),
                  ),
                  Button(
                    "Blank",
                    () => components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ShellSet(Blank)))),
                  ),
                  undoButton(() => components.update(shotSet.copy(input = InputTarget.ChooseTarget))),
                )

              case InputShell.ShellSet(shell) =>
                Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                  submitButton(() => submit.event(Shot(game.turn, target, shell))),
                  undoButton(() =>
                    components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ChooseShell))),
                  ),
                )
            }
        }

      case usedSet @ Input.UsedSet(input) =>
        input match {
          case InputItem.ChooseItem =>
            Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
              Panel(LinearLayout(Direction.VERTICAL)).withSeq {
                val items = game.actor.items.asSet.toVector.sortBy(_.toString)
                items.map:
                  case Adrenaline =>
                    Button(
                      "Adrenaline",
                      () =>
                        components.update(usedSet.copy(input = InputItem.AdrenalineSet(InputItemToSteal.ChooseItem))),
                    )
                  case Handcuffs =>
                    Button(
                      "Handcuffs",
                      () => components.update(usedSet.copy(input = InputItem.SimpleItemSet(Handcuffs))),
                    )
                  case MagnifyingGlass =>
                    Button(
                      "Magnifying Glass",
                      () =>
                        components.update(
                          usedSet.copy(input =
                            InputItem.MagnifyingGlassSet(
                              MagnifyingGlass,
                              Option.when(game.turn == Player)(InputShell.ChooseShell),
                            ),
                          ),
                        ),
                    )
                  case Beer =>
                    Button(
                      "Beer",
                      () => components.update(usedSet.copy(input = InputItem.BeerSet(Beer, InputShell.ChooseShell))),
                    )
                  case Cigarettes =>
                    Button(
                      "Cigarettes",
                      () => components.update(usedSet.copy(input = InputItem.SimpleItemSet(Cigarettes))),
                    )
                  case Saw =>
                    Button(
                      "Saw",
                      () => components.update(usedSet.copy(input = InputItem.SimpleItemSet(Saw))),
                    )
                  case Inverter =>
                    Button(
                      "Inverter",
                      () => components.update(usedSet.copy(input = InputItem.SimpleItemSet(Inverter))),
                    )
                  case BurnerPhone =>
                    Button(
                      "Burner Phone",
                      () =>
                        components.update(
                          usedSet.copy(input =
                            InputItem.BurnerPhoneSet(
                              BurnerPhone,
                              Option.when(game.turn == Player)(InputSeqNr.ChooseSeqNr),
                            ),
                          ),
                        ),
                    )
                  case Meds =>
                    Button(
                      "Meds",
                      () =>
                        components.update(usedSet.copy(input = InputItem.MedsSet(Meds, InputMedsQuality.ChooseQuality))),
                    )
              },
              undoButton(() => components.update(Input.ChooseEvent)),
            )
          case InputItem.SimpleItemSet(item) => ???
          case InputItem.MagnifyingGlassSet(item, input) => ???
          case InputItem.BeerSet(item, input) => ???
          case InputItem.BurnerPhoneSet(item, input) => ???
          case InputItem.MedsSet(item, input) => ???
          case InputItem.AdrenalineSet(input) => ???
        }
    }

  private def undoButton(callback: () => Unit): Component =
    Button("undo", () => callback()).withBorder(Borders.singleLine())

  private def submitButton(callback: () => Unit): Component =
    Button("submit", () => callback()).withBorder(Borders.singleLine())

  private def command(game: TableState, input: Input)(using Opposition[TableState]): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withSeq(
      Label(game.turn.toString) :: (input match {
        case Input.ChooseEvent => List(Label("_"))

        case Input.ShotSet(input, undoDisabled) =>
          Label("shot") :: (input match {
            case InputTarget.ChooseTarget => List(Label("_"))
            case InputTarget.TargetSet(target, input) =>
              Label(target.toString) :: Label("with") :: List(input match {
                case InputShell.ChooseShell => Label("_")
                case InputShell.ShellSet(shell) => Label(shell.toString)
              })
          })

        case Input.UsedSet(input) =>
          Label("used") :: (input match {
            case InputItem.ChooseItem => List(Label("_"))
            case InputItem.SimpleItemSet(item) => List(Label(item.toString))
            case InputItem.MagnifyingGlassSet(item, input) =>
              Label("Magnifying Glass") :: (input match {
                case Some(input) =>
                  Label("and revealed") :: List(input match {
                    case InputShell.ChooseShell => Label("_")
                    case InputShell.ShellSet(shell) => Label(shell.toString)
                  })
                case None => Nil
              })
            case InputItem.BeerSet(item, input) =>
              Label("Beer") :: Label("and removed") :: List(input match {
                case InputShell.ChooseShell => Label("_")
                case InputShell.ShellSet(shell) => Label(shell.toString)
              })
            case InputItem.BurnerPhoneSet(item, input) =>
              Label("Burner Phone") :: (input match {
                case Some(input) =>
                  Label("and on spot") :: (input match {
                    case InputSeqNr.ChooseSeqNr => List(Label("_"))
                    case InputSeqNr.SeqNrSet(seqNr, input) =>
                      Label(seqNr.toString) :: Label("revealed") :: List(input match {
                        case InputShell.ChooseShell => Label("_")
                        case InputShell.ShellSet(shell) => Label(shell.toString)
                      })
                  })
                case None => Nil
              })
            case InputItem.MedsSet(item, input) =>
              Label("Meds") :: Label("and") :: List(input match {
                case InputMedsQuality.ChooseQuality => Label("_")
                case InputMedsQuality.QualitySet(good) => Label(if good then "healed" else "damaged")
              })

            case InputItem.AdrenalineSet(input) =>
              Label("Adrenaline") :: Label("and stole") :: (input match {
                case InputItemToSteal.ChooseItem => List(Label("_"))
                case InputItemToSteal.SimpleItemSet(item) => List(Label(item.toString))
                case InputItemToSteal.MagnifyingGlassSet(item, input) =>
                  Label("Magnifying Glass") :: (input match {
                    case Some(input) =>
                      Label("and revealed") :: List(input match {
                        case InputShell.ChooseShell => Label("_")
                        case InputShell.ShellSet(shell) => Label(shell.toString)
                      })
                    case None => Nil
                  })
                case InputItemToSteal.BeerSet(item, input) =>
                  Label("Beer") :: Label("and removed") :: List(input match {
                    case InputShell.ChooseShell => Label("_")
                    case InputShell.ShellSet(shell) => Label(shell.toString)
                  })
                case InputItemToSteal.BurnerPhoneSet(item, input) =>
                  Label("Burner Phone") :: (input match {
                    case Some(input) =>
                      Label("and on spot") :: (input match {
                        case InputSeqNr.ChooseSeqNr => List(Label("_"))
                        case InputSeqNr.SeqNrSet(seqNr, input) =>
                          Label(seqNr.toString) :: Label("revealed") :: List(input match {
                            case InputShell.ChooseShell => Label("_")
                            case InputShell.ShellSet(shell) => Label(shell.toString)
                          })
                      })
                    case None => Nil
                  })
                case InputItemToSteal.MedsSet(item, input) =>
                  Label("Meds") :: Label("and") :: List(input match {
                    case InputMedsQuality.ChooseQuality => Label("_")
                    case InputMedsQuality.QualitySet(good) => Label(if good then "healed" else "damaged")
                  })
              })
          })
      }),
    )
