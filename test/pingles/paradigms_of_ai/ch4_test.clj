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
  (testing "goal is subset of current state"
    (is (= #{:eating}
           (achieve [] #{:eating} :eating)))
    (is (= #{:eating :happy}
           (achieve [] #{:eating :happy} :eating)))
    (is (= #{:eating}
           (achieve [] #{:eating} :pay))))
  (testing "goal is achievable through applicable goal"
    (is (= #{:standing}
           (achieve [(make-op :stand
                               :preconditions #{:sitting}
                               :add-list [:standing]
                               :del-list [:sitting])]
                     #{:sitting}
                     :standing))))
  (testing "goal is achievable through nested operation"
    (is (solved? #{:standing}
                 (achieve [(make-op :stand
                                     :preconditions #{:stretch}
                                     :add-list [:standing]
                                     :del-list [:sitting])
                            (make-op :stretching
                                     :preconditions #{:sitting}
                                     :add-list [:stretch])]
                           #{:sitting}
                           :standing)))
    (is (= #{:standing :stretch}
           (achieve [(make-op :stand
                               :preconditions #{:stretch}
                               :add-list [:standing]
                               :del-list [:sitting])
                      (make-op :stretching
                               :preconditions #{:sitting}
                               :add-list [:stretch])]
                     #{:sitting}
                     :standing)))))

(deftest applying-ops
  (let [op (make-op :pay :add-list [:paid] :del-list [:eating])]
    (is (= #{:paid}
           (apply-op #{:eating}
                     op)))
    (is (= #{:eating}
           (apply-op #{:eating}
                     (assoc op :preconditions #{:have-money}))))))

(deftest general-problem-solver
  (testing "solvable problems"
    (is (= #{:home}
           (gps #{:home} #{:home} [])))
    (is (solved? #{:home}
                 (gps #{:home} #{:home} [])))
    (is (= #{:son-at-school :car-works}
           (gps #{:son-at-home :car-works}
                #{:son-at-school}
                school-ops)))
    (is (solved? #{:son-at-school}
                 (gps #{:son-at-home :car-works}
                      #{:son-at-school}
                      school-ops))) 
    (is (solved? #{:son-at-school :car-works}
                 (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                      #{:son-at-school}
                      school-ops))))
  (testing "unsolvable problem"
    (is (= #{:shop-has-money :car-needs-battery :son-at-home}
           (gps #{:son-at-home :car-needs-battery :have-money}
                #{:son-at-school}
                school-ops)))
    (is (not (solved? #{:son-at-school}
                      (gps #{:son-at-home :car-needs-battery :have-money}
                           #{:son-at-school}
                           school-ops))))))
