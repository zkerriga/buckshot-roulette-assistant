package com.zkerriga.buckshot.game.state

package object items {
  type Adrenaline = Adrenaline.type
  type Item = RegularItem | Adrenaline

  export RegularItem.*
}
