"Utility functions and other frequently imported components."


(defclass ActionError [Exception]
  "Represents an attempt by the player to make an illegal move, such as walking through a wall.")

(defclass GameOverException [Exception]
  "Represents a game ending, whether by losing or winning.")
