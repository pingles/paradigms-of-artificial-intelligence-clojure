(ns pingles.paradigms-of-ai.ch4)

(defrecord Op [action precond add-list del-list])

(defn make-op
  [action & kvs]
  (map->Op (apply hash-map (concat kvs [:action action]))))

(defn member?
  [x coll]
  (not (nil? (some #{x} coll))))

(defn achieve
  "A goal is acheived if it already holds or if there is an appropriate op
   for it that is applicable"
  [state operators goal]
  (member? goal state))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add-list"
  [goal op]
  (member? goal (:add-list op)))

(defn apply-op
  "Applies the operation: adding and removing states when op is applicable."
  [state op]
  )

(defn gps
  "General Problem Solver: achieve all goals using ops"
  [state goals ops]
  (if (every? (partial achieve state ops) goals)
    :solved))

(comment
  (def school-ops [(make-op :drive-son-to-school
                            :precond #{:son-at-home :car-works}
                            :add-list [:son-at-school]
                            :del-list [:son-at-home])
                   (make-op :shop-installs-battery
                            :precond #{:car-needs-battery :shop-knows-problem :shop-has-money}
                            :add-list [:car-works])
                   (make-op :tell-shop-problem
                            :precond #{:in-communication-with-shop}
                            :add-list [:shop-knows-problem])
                   (make-op :telephone-shop
                            :precond #{:know-phone-number}
                            :add-list [:in-communication-with-shop])
                   (make-op :look-up-number
                            :precond #{:have-phone-book}
                            :add-list [:know-phone-number])
                   (make-op :give-shop-money
                            :precond #{:have-money}
                            :add-list [:shop-has-money]
                            :del-list [:have-money])])

  (gps #{:son-at-home :car-works}
       #{:son-at-school}
       school-ops))
