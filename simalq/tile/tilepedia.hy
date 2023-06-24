(import
  simalq.color
  simalq.game-state [G]
  simalq.tile [Tile Scenery Item Monster Player]
  simalq.display [color-tile]
  simalq.quest [Quest start-quest])
(setv  T True  F False)


(defn html []
  "Compile all the tile info screens into an HTML document."

  (import
    ; The depenency on `lxml` isn't declared in `setup.py` because it
    ; isn't needed to play the game.
    lxml.html
    lxml.builder)

  (start-quest None)  ; Initialize `G`.
  (setv tiles (lfor
    tt (.values Tile.types)
    (if (is tt Player) G.player (tt))))

  (setv E (lxml.builder.ElementMaker
    :makeelement lxml.html.html-parser.makeelement))
  (setv superclasses (do-mac (lfor
    c '[Scenery Item Monster Player]
    [(str c) c])))

  (defn mapsym [tile]
    (E.code :class "mapsym" #* (gfor
      cc (color-tile tile)
      (E.span cc.char :style
        (.format "color: {}; background-color: {}" #* (gfor
          color [
            (or cc.fg simalq.color.default-fg)
            (or cc.bg simalq.color.default-bg)]
          (.format "#{:02x}{:02x}{:02x}" #*
            (get simalq.color.by-name color))))))))

  (setv doc (E.html :lang "en"

    ; The `<head>`
    (E.head
      (E.meta :charset "UTF-8")
      (E.title "Tilepedia — Infinitesimal Quest 2 + ε")
      (E.style "
        h2, h3
           {border-top: thin solid}
        .mapsym
           {white-space: pre;
            margin-right: 1em}
        .flavor
           {white-space: pre-wrap}"))

    ; The page header
    (E.h1 "Tilepedia")
    (E.p
      "This page is a compendium of info screens for every tile type in "
      (E.a :href "https://hylang.org/simalq" "Infinitesimal Quest 2 + ε")
      ". It's generated by "
      (E.code "simalq.tile.tilepedia.html")
      ". It reflects default values for each type; "
      "in game, for example, monsters can have more than 1 HP.")

    ; The table of contents
    (E.nav
      (E.h2 "Contents")
      (E.ul #* (gfor
        [name superclass] superclasses
        (E.li name (E.ul #* (gfor
          tile tiles
          :if (isinstance tile superclass)
          (E.li (mapsym tile) (E.a tile.full-name
            :href (+ "#" (.replace tile.stem " " "-"))))))))))

    ; One section of info screens per superclass
    #* (cat (gfor
      [name superclass] superclasses
      [(E.h2 name) #* (cat (gfor
        ; One info screen per tile type
        tile tiles
        :if (isinstance tile superclass)
        [(E.h3 (mapsym tile) tile.full-name :id (.replace tile.stem " " "-"))
          (E.ul #* (gfor
            bullet (.info-bullets tile)
            :if bullet
            (E.li #* (if (isinstance bullet tuple)
              [(E.strong (get bullet 0) ": ") (str (get bullet 1))]
              [(E.strong bullet)]))))
          (E.div :class "flavor" tile.flavor)]))]))))

  (lxml.html.tostring doc
    :pretty-print T :encoding "unicode" :doctype "<!DOCTYPE html>"))


(defn cat [l]
  (sum :start [] l))


(when (= __name__ "__main__")
  (print (html)))
