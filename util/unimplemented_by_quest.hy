"Count unimplemented tile types per IQ quest. With a quest name
as a command-line argument, show which unimplemented tiles appear on
each level."


(import
  sys
  hy.pyops *
  pandas :as pd
  simalq.un-iq [read-quest iq-quest]
  simalq.tile.unimplemented [UnimplementedTile])


(setv qts (dfor
  [qname v] (.items (iq-quest 'all))
  :setv q (read-quest v)
  qname (lfor  l q.levels  (sorted (sfor
    column l.map.data
    stack column
    tile stack
    :if (isinstance tile UnimplementedTile)
    tile.stem)))))


(if (> (len sys.argv) 1)
  (do
    (setv v (get qts (get sys.argv 1)))
    (print "Tile types not implemented per level:")
    (for [[i stems] (enumerate v)]
      (print (+ i 1) stems)))
  (do
    (print "Distinct tile types not implemented:")
    (print (.sort-values (pd.concat (gfor
      [q v] (.items qts)
      (pd.Series  :index [q]  (len (set (+ #* v))))))))))
