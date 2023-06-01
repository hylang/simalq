(setv

  by-name (dfor
    [k v] (.items {

      'black [0 0 0]
      'white [1 1 1]
      'dark-gray [(/ 1 4) (/ 1 4) (/ 1 4)]
      'light-gray [(/ 3 4) (/ 3 4) (/ 3 4)]
      'brown [(/ 1 2) (/ 1 4) 0]
      'yellow [1 1 0]
      'pale-yellow [1 1 (/ 3 4)]
      'dark-yellow [(/ 1 2) (/ 1 2) 0]
      'pale-red [1 (/ 7 8) (/ 7 8)]
      'red [(/ 3 4) 0 0]
      'orange [1 (/ 1 2) 0]
      'dark-orange [(/ 3 4) (/ 1 2) 0]
      'pale-blue [(/ 7 8) (/ 7 8) 1]
      'dark-navy [0 0 (/ 3 8)]
      'navy [0 0 (/ 1 2)]
      'rose [1 0 (/ 1 2)]
      'pale-magenta [1 (/ 7 8) 1]
      'magenta [1 (/ 1 4) 1]
      'purple [(/ 1 2) 0 (/ 1 2)]
      'blue [0 0 1]
      'steel-blue [(/ 1 4) (/ 1 2) (/ 3 4)]
      'pale-green [(/ 7 8) 1 (/ 7 8)]
      'moss-green [(/ 1 2) (/ 3 4) (/ 1 2)]
      'dark-green [0 (/ 1 4) 0]
      'lime [0 1 0]})

    k (tuple (gfor  x v  (round (* x 255)))))

  default-fg 'black
  default-bg 'white

  void 'dark-navy
  reality-fringe 'light-gray
  reality-fringe-block 'dark-gray
  overwrapped 'dark-gray
  focus 'yellow
  message-bg 'pale-yellow

  flash-label 'white
  flash-player-damaged 'red
  flash-player-shot-mundane 'blue
  flash-player-shot-magic 'purple

  tile-bg-by-hp {
    1       'pale-green
    2       'pale-blue
    3       'pale-red
    "other" 'pale-magenta})
