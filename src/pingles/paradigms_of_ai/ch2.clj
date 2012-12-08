(ns pingles.paradigms-of-ai.ch2)

(def ^:dynamic *grammar* '{sentence [[noun-phrase verb-phrase]]
                           noun-phrase [[Article Noun]]
                           verb-phrase [[Verb noun-phrase]]
                           Article [the a]
                           Noun [man ball woman table]
                           Verb [hit took saw liked]})

(def bigger-grammar '{sentence [[noun-phrase verb-phrase]]
                      noun-phrase [[Article Adj* Noun PP*] [Name] [Pronoun]]
                      verb-phrase [[Verb noun-phrase PP*]]
                      PP* [[] [PP PP*]]
                      Adj* [[] [Adj Adj*]]
                      PP [[Prep noun-phrase]]

                      Prep [to in by with on]
                      Adj [big little blue green adiabatic]
                      Article [the a]
                      Name [Pat Kim Lee Terry Robin]
                      Noun [man ball woman table]
                      Verb [hit took saw liked]
                      Pronoun [he she it these those that]})

(defmacro with-grammar [grammar & body]
  `(binding [*grammar* ~grammar]
     ~@body))

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

  (with-grammar bigger-grammar
    (generate 'sentence))
  ;(a man liked those)
  )
