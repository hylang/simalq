(require
  simalq.macros [defdataclass]
  hyrule [unless])
(import
  simalq.util [ActionError]
  simalq.geometry [Direction GeometryError pos+]
  simalq.tile [Tile deftile rm-tile replace-tile]
  simalq.game-state [G])
(setv  T True  F False)


(defdataclass Scenery [Tile]
  "A mostly static part of the level, such as a wall or trap."
  []

  (setv
    blocks-move F
      ; Block movement.
    blocks-diag F))
      ; Block diagonal movement between orthogonally adjacent squares.


(deftile Scenery "a wall"
  :iq-ix 2
  :blocks-move T :blocks-diag T
  :flavor (.join "\n" [
    "Among the most numerous and persistent of the obstacles that stand in the way of your inevitable victory."
    ""
    "  This man, with lime and rough-cast, doth present"
    "  Wall, that vile Wall which did these lovers sunder;"
    "  And through Wall's chink, poor souls, they are content"
    "  To whisper, at the which let no man wonder."]))

(deftile Scenery "a pillar"
  :iq-ix 12
  :blocks-move T
  :flavor "A structure of vaguely Roman style.")

(deftile Scenery "a door"
  :iq-ix 5
  :flavor "Unlocked, but it just won't stay open. Maybe that's for the best, since monsters are too dumb to operate it.")

(defdataclass LockedDoor [Scenery]
  []
  (setv destroy-when-opened None)
  (defn hook-player-walk-to [self origin]
    (unless G.keys
      (raise (ActionError "It's locked, and you're keyless at the moment.")))
    (-= G.keys 1)
    (if self.destroy-when-opened
      (rm-tile self)
      (replace-tile self "door"))
    True))

(deftile LockedDoor "a locked door"
  :iq-ix 6
  :destroy-when-opened False
  :flavor "Fortunately, Tris knows how to pick locks. Unfortunately, she was wearing her hair down when she got whisked away to the dungeon, so she doesn't have any hairpins. You may have to use a key.")

(deftile LockedDoor "a locked disappearing door"
  :iq-ix 81
  :destroy-when-opened True
  :flavor "This advanced door destroys not only the key used to unlock it, but also itself. A true marvel of engineering.")

((fn []

  (defn safe-pos+ [pos direction]
    (try
      (pos+ pos direction)
      (except [GeometryError])))

  (defdataclass OneWayDoor [Scenery]
    []

    (setv direction None)

    (defn hook-player-walk-from [self target]
      (unless (= (safe-pos+ self.pos self.direction) target)
        (raise (ActionError f"You can only go {self.direction.name} from this one-way door."))))
    (defn hook-player-walk-to [self origin]
      (unless (= (safe-pos+ origin self.direction) self.pos)
        (raise (ActionError (.format "That one-way door must be entered from the {}."
          self.direction.opposite.name)))))

    (setv flavor "My way or the highway!"))

  (for [[direction iq-ix] [
      [Direction.N 8] [Direction.E 11]
      [Direction.S 9] [Direction.W 10]]]
    (deftile OneWayDoor f"a one-way door ({direction.name})"
      :iq-ix iq-ix
      :direction direction))))
