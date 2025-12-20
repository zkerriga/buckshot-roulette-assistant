# buckshot-roulette-assistant
An attempt to build an algorithm that would help make better decisions against dealer in [Buckshot Roulette](https://store.steampowered.com/app/2835570/Buckshot_Roulette/) game

### Expected Flow

Input Initial State:
- max health
- player health
- dealer health
- shotgun state
- player items
- dealer items
- turn state

if it's dealer turn:
the app should show a sequence of expected dealer actions until the turn ends

* dealer uses Cigarettes via Adrenaline
* dealer uses Beer
  * if live:
    * dealer shoots player
    * ...
  * if blank:
    * dealer shoots self
    * ...

for each sequence of dealer actions in the end there is a state of the game (known)

I should be able to type real game events like:
* dealer used Cigarettes via Adrenaline
* dealer used Beer `Live` left - meaning that the actually there was a Live round in the shotgun, all branches that do not correspond to that, should be eliminated and dealer actions output should be adjusted accordingly
* dealer shot Player with `Blank`

Similarly, when it's player turn:
I should be able to type real game events like:
* I used Beer, `Blank` left
* I shot Dealer, `Live`

the app should show a sequence of best actions for the player to take
since there could be multiple states after dealer turn, the app should search for actions that result in win unconditionally, or maximize the chances of winning or resetting
