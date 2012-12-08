(ns pingles.paradigms-of-ai.ch2)

(def ^:dynamic *grammar* '{sentence [[noun-phrase verb-phrase]]
                           noun-phrase [[Article Noun]]
                           verb-phrase [[Verb noun-phrase]]
                           Article [the a]
                           Noun [man ball woman table]
                           Verb [hit took saw liked]})

(defn rule-lhs
  [rule]
  (first rule))

(defn rewrites
  [category]
  (get *grammar* category))

(defn generate
  "Creates a random sentence or phrase"
  [phrase]
  (cond (coll? phrase) (mapcat generate phrase)
        (rewrites phrase) (generate (rand-nth (rewrites phrase)))
        :else (list phrase)))

(comment
  (generate 'sentence)
  ; a man took a ball
  )
