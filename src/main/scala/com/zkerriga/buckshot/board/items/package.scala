package com.zkerriga.buckshot.board

package object items {
  type Adrenaline = Adrenaline.type
  type Item = RegularItem | Adrenaline

  export RegularItem.*
}
