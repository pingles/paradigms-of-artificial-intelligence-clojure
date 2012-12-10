(ns pingles.paradigms-of-ai.ch4-test
  (:use [clojure.test]
        [pingles.paradigms-of-ai.ch4] :reload))

(deftest membership
  (is (member? 1 [1 2 3])))

(deftest op-appropriateness
  (is (appropriate? :eating
                    (make-op :visit-restaurant :add-list [:eating])))
  (is (not (appropriate? :eating
                         (make-op :visit-library :add-list [:reading])))))

(deftest achieving-goals
  (is (true? (achieve #{:eating} [] :eating)))
  (is (false? (achieve #{:eating} [] :sleeping)))
  (is (true? (achieve #{:sitting}
                      [(make-op :stand :add-list [:standing])]
                      :standing))))

(deftest applying-ops
  (let [op (make-op :pay :add-list [:paid] :del-list [:eating])]
    (is (= #{:paid}
           (apply-op #{:eating}
                     op)))
    (is (= #{:eating}
           (apply-op #{:eating}
                     (assoc op :precond #{:have-money}))))))

(deftest general-problem-solver
  (is (= :solved
         (gps #{:home} #{:home} [])))
  (is (= :solved
         (gps #{:home} #{:eating} [(make-op :walk-to-restaurant
                                            :precond #{:home}
                                            :add-list [:eating]
                                            :del-list [:home])])))
  (is (= :solved
         (gps #{:son-at-home :car-works}
              #{:son-at-school}
              school-ops)))
  (is (= :solved
         (gps #{:son-at-home :car-works}
              #{:son-at-school}
              school-ops)))
  (is (nil? (gps #{:son-at-home :car-needs-battery :have-money}
              #{:son-at-school}
              school-ops))))
