(ns lcr-clojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def mymap {:a 1 :b 2 :c 3})
(defn default-value
  []
  (:d mymap "no gnome knows homes like Noah knows"))

(defn rollSixSidedDice
  []
  (+ 1 (int (rand 6))))

(defn roll
  [numOfDice]
  (defn diceHelper
    [vecDice dieRemaining]
    (if (> dieRemaining 0)
      (recur (conj vecDice (rollSixSidedDice)) (- dieRemaining 1))
      vecDice))
  (diceHelper [] numOfDice))

(defn rollCLRDie
  [numOfDice]
  (defn numToCLR
    [num]
    (if (or (= num 1) (= num 3) (= num 5))
      "S"
      (if (= num 2)
        "L"
        (if (= num 4)
          "C"
          (if (= num 6)
            "R")))))
  (defn clrHelper
    [vecDice vecClr]
    (if (empty? vecDice)
      vecClr
      (recur (pop vecDice) (conj vecClr (numToCLR (last vecDice)))))
    )
  (clrHelper (roll numOfDice) []))

(defn readDie
  "Returns [:quartersToLeft :quartersToCenter :quartersToRight]"
  [clrResults]
  (defn helper
    [vecClr quartersToLeft quartersToCenter quartersToRight]
    (if (empty? vecClr)
      [quartersToLeft quartersToCenter quartersToRight]
      (let [result (last vecClr)]
        (if (= result "L")
          (helper (pop vecClr) (+ quartersToLeft 1) quartersToCenter quartersToRight)
          (if (= result "C")
            (helper (pop vecClr) quartersToLeft (+ quartersToCenter 1) quartersToRight)
            (if (= result "R")
              (helper (pop vecClr) quartersToLeft quartersToCenter (+ quartersToRight 1))
              (helper (pop vecClr) quartersToLeft quartersToCenter quartersToRight)))))))
  (helper clrResults 0 0 0)
  )

(defn turn
  [players currentPlayerIndex eventLog turnNum]
  (def potIndex (- (count players) 1))
  (def playersEndIndex (- potIndex 1))
  (def leftPlayerIndex
    (if (= currentPlayerIndex 0)
      playersEndIndex
      (- currentPlayerIndex 1)))
  (def rightPlayerIndex
    (if (= currentPlayerIndex playersEndIndex)
      0
      (+ currentPlayerIndex 1)))
  (def leftPlayer (get players leftPlayerIndex))
  (def rightPlayer (get players rightPlayerIndex))
  (def pot (get players potIndex))
  (def currentPlayer (get players currentPlayerIndex))
  (def currentPlayerScore (get currentPlayer 1))
  (def playerRoll
    (if (> currentPlayerScore 2)
      (rollCLRDie 3)
      (rollCLRDie currentPlayerScore)))
  (def playerRollResult (readDie playerRoll))

  (defn updatePlayerScore
    [player scoreChange]
    (assoc player 1 (+ (get player 1) scoreChange)))

  (defn updatePlayersScores
    [leftChange centerChange rightChange]
    (assoc players
           leftPlayerIndex (updatePlayerScore leftPlayer leftChange)
           potIndex (updatePlayerScore pot centerChange)
           rightPlayerIndex (updatePlayerScore rightPlayer rightChange)
           currentPlayerIndex (updatePlayerScore currentPlayer (* (+ leftChange centerChange rightChange) -1))))

  (def updatedScores (updatePlayersScores (get playerRollResult 0)
                                          (get playerRollResult 1)
                                          (get playerRollResult 2)))
  (defn genEvent
    []
    ; (str "Roll " (+ (count eventLog) 1) ": " (get currentPlayer 0) "\n" playerRoll "\n" updatedScores "\n\n"))
    (str "Roll " turnNum ": " (get currentPlayer 0) "\n" playerRoll "\n" updatedScores "\n\n"))

  ; (def updatedEventLog (conj eventLog (genEvent)))
                                        ; [updatedScores updatedEventLog]
  [updatedScores (genEvent) turnNum]
  )

(defn gameRound
  [players]
  (def playersEndIndex (- (count players) 1))
  (defn checkForWinner
    [players]
    (defn cfwHelper
      [uncheckedPlayers numOfPlayersWithPoints winningIndex]
      (if (empty? uncheckedPlayers)
        [numOfPlayersWithPoints winningIndex]
        (let [checkedPlayer (peek uncheckedPlayers)
              checkedPlayerIndex (- (count uncheckedPlayers) 1)
              isPlayerPointless (= (get checkedPlayer 1) 0)]
          (cfwHelper (pop uncheckedPlayers)
                     (if isPlayerPointless
                       numOfPlayersWithPoints
                       (inc numOfPlayersWithPoints))
                     (if isPlayerPointless
                       winningIndex
                       checkedPlayerIndex)))))

    (let [result (cfwHelper players 0 0)]
      (if (> (get result 0) 1)
        false
        true)))

  (defn roundHelper
    [turnResults currentPlayerIndex]
    (def scores (get turnResults 0))
    (def events (get turnResults 1))
    (def turnNum (get turnResults 2))
    ; (print (peek events))
    ; (print events)
    (def newCurrentPlayerIndex
      (if (= currentPlayerIndex playersEndIndex)
        0
        (+ currentPlayerIndex 1)))
    (if (checkForWinner (pop scores))
      (print events)
      (recur (turn scores newCurrentPlayerIndex events (inc turnNum)) newCurrentPlayerIndex)))

  (roundHelper (turn (conj players ["Pot" 0]) 0 [] 1) 0))

(def players [["Brad" 10] ["Kristina" 10] ["Ruby" 10] ["Dean" 10]])

(defn rollit [num] (readDie (rollCLRDie num)))
