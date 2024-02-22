"This file binds keys to keyboard commands. Edit it to rebind keys."


(import
  simalq.geometry [Direction :as D]
  simalq.commands *)


(setv direction-keys {
  "7/y/HOME" D.NW  "8/k/UP"     D.N      "9/u/PGUP"   D.NE
  "4/h/LEFT" D.W   "5/./ESCAPE" 'center  "6/l/RIGHT"  D.E
  "1/b/END"  D.SW  "2/j/DOWN"   D.S      "3/n/PGDOWN" D.SE})
(setv command-keys {
  "?" Help
  "!" Quit
  "S" [SaveGame 'main]
  "C" [SaveGame 'checkpoint]
  "L" LoadGame
  "e" [ShiftHistory  -1]  ; Undo
  "E" [ShiftHistory -10]  ; Undo several times
  "r" [ShiftHistory  +1]  ; Redo
  "R" [ShiftHistory +10]  ; Redo several times
  ";" Look
  "f" GonnaShoot
  "i" Inventory
  "a" GonnaUseItem})


(setv direction-keys (dfor
  :setv D Direction
  [k v] (.items direction-keys)
  s (.split k "/")
  (if (> (len s) 1) (+ "KEY_" s) s) v))
(defn read-dir-key [key]
  (.get direction-keys (str key) (.get direction-keys key.name)))
