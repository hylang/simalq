"Count unimplemented tile types per IQ quest."


(import
  pandas :as pd
  simalq.un-iq [read-quest iq-quest]
  simalq.tile.unimplemented [UnimplementedTile])


(setv qts (dfor
  [qname v] (.items (iq-quest 'all))
  :setv q (read-quest v)
  qname (sorted (sfor
    l q.levels
    column l.map.data
    stack column
    tile stack
    :if (isinstance tile UnimplementedTile)
    tile.stem))))

(print "Distinct tile types not implemented:")
(print (.sort-values (pd.concat (gfor
  [q v] (.items qts)
  (pd.Series  :index [q]  (len v))))))

;(print (.join " • " (get qts "New First Quest")))
