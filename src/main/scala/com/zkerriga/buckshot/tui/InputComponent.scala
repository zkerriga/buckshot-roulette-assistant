package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.events.PlayerUsed
import com.zkerriga.buckshot.game.accessors.Opposition
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.{ItemUse, execute}
import com.zkerriga.buckshot.game.events.{Shot, Used}
import com.zkerriga.buckshot.game.state.partitipant.Side
import com.zkerriga.buckshot.game.state.shotgun

object InputComponent:
  trait Submit:
    def event(event: Engine.Event): Unit

  def render(game: TableState, submit: Submit): Component =
    given Opposition[TableState] = game.turn match
      case Player => Opposition.of(actorIs = _.player, opponentIs = _.dealer)
      case Dealer => Opposition.of(actorIs = _.dealer, opponentIs = _.player)
    DynamicComponent
      .selfUpdatableOnly(Input.from(game)) { (input, update) =>
        Panel(GridLayout(2)).withAll(
          Label("Command"),
          command(game, input),
          EmptySpace(),
          buttons(game, input, update, submit),
        )
      }
      .withBorder(Borders.singleLine("Input"))

  private enum Input:
    case ChooseEvent
    case ShotSet(input: InputTarget, undoDisabled: Boolean)
    case UsedSet(input: InputItem)

  private enum InputTarget:
    case ChooseTarget
    case TargetSet(target: Side, input: InputShell)

  private enum InputShell:
    case ChooseShell
    case ShellSet(shell: Shell) // end of input

  private enum InputItem:
    case ChooseItem
    case SimpleItemSet(item: Handcuffs.type | Cigarettes.type | Saw.type | Inverter.type) // end of input
    case MagnifyingGlassSet(item: MagnifyingGlass.type, input: Option[InputShell])
    case BeerSet(item: Beer.type, input: InputShell)
    case BurnerPhoneSet(item: BurnerPhone.type, input: Option[InputSeqNr])
    case MedsSet(item: Meds.type, input: InputMedsQuality)
    case AdrenalineSet(input: InputItemToSteal)

  private enum InputItemToSteal:
    case ChooseItem
    case SimpleItemSet(item: Handcuffs.type | Cigarettes.type | Saw.type | Inverter.type) // end of input
    case MagnifyingGlassSet(item: MagnifyingGlass.type, input: Option[InputShell])
    case BeerSet(item: Beer.type, input: InputShell)
    case BurnerPhoneSet(item: BurnerPhone.type, input: Option[InputSeqNr])
    case MedsSet(item: Meds.type, input: InputMedsQuality)

  private enum InputSeqNr:
    case ChooseSeqNr
    case SeqNrSet(seqNr: SeqNr, input: InputShell)

  private enum InputMedsQuality:
    case ChooseQuality
    case QualitySet(good: Boolean) // end of input

  private object Input:
    def from(game: TableState)(using Opposition[TableState]): Input =
      if game.actor.items.empty then Input.ShotSet(InputTarget.ChooseTarget, undoDisabled = true)
      else Input.ChooseEvent

  private def buttons(
    game: TableState,
    input: Input,
    components: DynamicComponent.Update[Input],
    submit: Submit,
  )(using Opposition[TableState]): Panel =
    input match {
      case Input.ChooseEvent =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          button("Shot") {
            components.update(Input.ShotSet(InputTarget.ChooseTarget, undoDisabled = false))
          },
          button("Used") {
            components.update(Input.UsedSet(InputItem.ChooseItem))
          },
        )

      case shotSet @ Input.ShotSet(input, undoDisabled) =>
        input match {
          case InputTarget.ChooseTarget =>
            Panel(LinearLayout(Direction.HORIZONTAL)).withSeq:
              Seq(
                button("Dealer") {
                  components.update(shotSet.copy(input = InputTarget.TargetSet(Dealer, InputShell.ChooseShell)))
                }.some,
                button("Player") {
                  components.update(shotSet.copy(input = InputTarget.TargetSet(Player, InputShell.ChooseShell)))
                }.some,
                Option.unless(undoDisabled)(undoButton {
                  components.update(Input.ChooseEvent)
                }),
              ).flatten

          case targetSet @ InputTarget.TargetSet(target, input) =>
            input match {
              case InputShell.ChooseShell =>
                Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                  liveButton {
                    components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ShellSet(Live))))
                  },
                  blankButton {
                    components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ShellSet(Blank))))
                  },
                  undoButton {
                    components.update(shotSet.copy(input = InputTarget.ChooseTarget))
                  },
                )

              case InputShell.ShellSet(shell) =>
                Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                  submitButton {
                    submit.event(Shot(game.turn, target, shell))
                  },
                  undoButton {
                    components.update(shotSet.copy(input = targetSet.copy(input = InputShell.ChooseShell)))
                  },
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
                    adrenalineButton {
                      components.update(usedSet.copy(input = InputItem.AdrenalineSet(InputItemToSteal.ChooseItem)))
                    }
                  case Handcuffs =>
                    handcuffsButton {
                      components.update(usedSet.copy(input = InputItem.SimpleItemSet(Handcuffs)))
                    }
                  case MagnifyingGlass =>
                    magnifyingGlassButton {
                      components.update(
                        usedSet.copy(input =
                          InputItem.MagnifyingGlassSet(
                            MagnifyingGlass,
                            Option.when(game.turn == Player)(InputShell.ChooseShell),
                          ),
                        ),
                      )
                    }
                  case Beer =>
                    beerButton {
                      components.update(usedSet.copy(input = InputItem.BeerSet(Beer, InputShell.ChooseShell)))
                    }
                  case Cigarettes =>
                    cigarettesButton {
                      components.update(usedSet.copy(input = InputItem.SimpleItemSet(Cigarettes)))
                    }
                  case Saw =>
                    sawButton {
                      components.update(usedSet.copy(input = InputItem.SimpleItemSet(Saw)))
                    }
                  case Inverter =>
                    inverterButton {
                      components.update(usedSet.copy(input = InputItem.SimpleItemSet(Inverter)))
                    }
                  case BurnerPhone =>
                    burnerPhoneButton {
                      components.update(
                        usedSet.copy(input =
                          InputItem.BurnerPhoneSet(
                            BurnerPhone,
                            Option.when(game.turn == Player)(InputSeqNr.ChooseSeqNr),
                          ),
                        ),
                      )
                    }
                  case Meds =>
                    medsButton {
                      components.update(usedSet.copy(input = InputItem.MedsSet(Meds, InputMedsQuality.ChooseQuality)))
                    }
              },
              undoButton {
                components.update(Input.ChooseEvent)
              },
            )
          case InputItem.SimpleItemSet(item) =>
            Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
              submitButton {
                game.turn match
                  case Player =>
                    val itemUse = item match
                      case Handcuffs => PlayerUsed.ItemUse.Handcuffs
                      case Cigarettes => PlayerUsed.ItemUse.Cigarettes
                      case Saw => PlayerUsed.ItemUse.Saw
                      case Inverter => PlayerUsed.ItemUse.Inverter
                    submit.event(PlayerUsed(itemUse, stolen = false))
                  case Dealer =>
                    val itemUse = item match
                      case Handcuffs => ItemUse.Handcuffs
                      case Cigarettes => ItemUse.Cigarettes
                      case Saw => ItemUse.Saw
                      case Inverter => ItemUse.Inverter
                    submit.event(Used(Dealer, itemUse, stolen = false))
              },
              undoButton {
                components.update(usedSet.copy(input = InputItem.ChooseItem))
              },
            )
          case glassSet @ InputItem.MagnifyingGlassSet(_, input) =>
            input match {
              case Some(input) =>
                input match {
                  case InputShell.ChooseShell =>
                    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                      liveButton {
                        components.update(usedSet.copy(input = glassSet.copy(input = InputShell.ShellSet(Live).some)))
                      },
                      blankButton {
                        components.update(usedSet.copy(input = glassSet.copy(input = InputShell.ShellSet(Blank).some)))
                      },
                      undoButton {
                        components.update(usedSet.copy(input = InputItem.ChooseItem))
                      },
                    )
                  case InputShell.ShellSet(shell) => ???
                }
              case None =>
                Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                  submitButton {
                    submit.event(Used(Dealer, ItemUse.MagnifyingGlass, stolen = false))
                  },
                  undoButton {
                    components.update(usedSet.copy(input = InputItem.ChooseItem))
                  },
                )
            }
          case InputItem.BeerSet(item, input) => ???
          case InputItem.BurnerPhoneSet(item, input) => ???
          case InputItem.MedsSet(item, input) => ???
          case InputItem.AdrenalineSet(input) => ???
        }
    }

  private val adrenalineButton = button("Adrenaline")
  private val handcuffsButton = button("Handcuffs")
  private val magnifyingGlassButton = button("Magnifying Glass")
  private val beerButton = button("Beer")
  private val cigarettesButton = button("Cigarettes")
  private val sawButton = button("Saw")
  private val inverterButton = button("Inverter")
  private val burnerPhoneButton = button("Burner Phone")
  private val medsButton = button("Meds")

  private val liveButton = button("Live")
  private val blankButton = button("Blank")

  private def undoButton(onClick: => Unit): Component =
    button("| undo |")(onClick)

  private def submitButton(onClick: => Unit): Component =
    button("| submit |")(onClick)

  private def button(name: String)(onClick: => Unit): Button = Button(name, () => onClick)

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
