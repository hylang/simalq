(setv

  by-name (dfor
    [k v] (.items {

      'black [0 0 0]
      'white [1 1 1]
      'dark-gray [(/ 1 4) (/ 1 4) (/ 1 4)]
      'light-gray [(/ 3 4) (/ 3 4) (/ 3 4)]
      'dark-navy [0 0 (/ 3 8)]})

    k (tuple (gfor  x v  (round (* x 255)))))

  default-fg 'black
  default-bg 'white

  void 'dark-navy
  reality-fringe 'light-gray
  reality-fringe-block 'dark-gray)
