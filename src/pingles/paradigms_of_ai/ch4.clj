(ns pingles.paradigms-of-ai.ch4
  (:use [clojure.set :only (intersection difference union)]))

(defrecord Op [action preconditions add-list del-list])

(defn executing-op
  [{:keys [add-list action] :as op}]
  (assoc op :add-list (conj add-list (list :executing action))))

(defn make-op
  [action & kvs]
  (-> (map->Op (apply hash-map (concat kvs [:action action])))
      (executing-op)))


(defn executing?
  [x]
  (and (sequential? x)
       (= :executing (first x))))

(defn member?
  [x coll]
  (not (nil? (some #{x} coll))))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add-list"
  [goal {:keys [add-list] :as op}]
  (member? goal add-list))

(defn apply-op
  "Applies the operation: adding and removing states when op is applicable."
  [state {:keys [preconditions] :as op}]
  (letfn [(update-state [state {:keys [del-list add-list]}]
            (-> state (difference del-list) (union add-list) (set)))]
    (cond (empty? (difference preconditions state)) (update-state state op)
          :else state)))

(defn achieve
  "A goal is acheived if it already holds or if there is an appropriate op
   for it that is applicable"
  ([operators state goal] (achieve operators [] state goal))
  ([operators goal-stack state goal]
     (cond (member? goal state) state
           (member? goal goal-stack) nil
           :else (reduce (fn [state {:keys [preconditions] :as op}]
                           (apply-op (reduce (partial achieve operators (conj goal-stack goal))
                                             state
                                             preconditions)
                                     op))
                         state
                         (filter (partial appropriate? goal) operators)))))

(defn gps
  "General Problem Solver: achieve all goals using ops"
  [current-state goals ops]
  (->> goals
       (reduce (partial achieve ops)
               current-state)
       (remove sequential?)
       (set)))

(defn solved?
  "Checks whether our state achieves all goals"
  [goals state]
  (empty? (difference goals state)))

(def school-ops [(make-op :drive-son-to-school
                          :preconditions #{:son-at-home :car-works}
                          :add-list [:son-at-school]
                          :del-list [:son-at-home])
                 (make-op :shop-installs-battery
                          :preconditions #{:car-needs-battery :shop-knows-problem :shop-has-money}
                          :add-list [:car-works])
                 (make-op :tell-shop-problem
                          :preconditions #{:in-communication-with-shop}
                          :add-list [:shop-knows-problem])
                 (make-op :telephone-shop
                          :preconditions #{:know-phone-number}
                          :add-list [:in-communication-with-shop])
                 (make-op :look-up-number
                          :preconditions #{:have-phone-book}
                          :add-list [:know-phone-number])
                 (make-op :give-shop-money
                          :preconditions #{:have-money}
                          :add-list [:shop-has-money]
                          :del-list [:have-money])])

(comment
  (solved? #{:son-at-school}
           (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                #{:son-at-school}
                school-ops))
  ;true
  )