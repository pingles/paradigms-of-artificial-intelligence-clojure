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
           (achieve-recur #{:eating} [] :eating)))
    (is (= #{:eating :happy}
           (achieve-recur #{:eating :happy} [] :eating)))
    (is (= #{:eating}
           (achieve-recur #{:eating} [] :pay))))
  (testing "goal is achievable through applicable goal"
    (is (= #{:standing}
           (achieve-recur #{:sitting}
                          [(make-op :stand
                                    :precond #{:sitting}
                                    :add-list [:standing]
                                    :del-list [:sitting])]
                          :standing)))))

(deftest applying-ops
  (let [op (make-op :pay :add-list [:paid] :del-list [:eating])]
    (is (= #{:paid}
           (apply-op #{:eating}
                     op)))
    (is (= #{:eating}
           (apply-op #{:eating}
                     (assoc op :precond #{:have-money}))))))

(deftest general-problem-solver
  (testing "solved problems"
    (is (= #{:home}
           (gps #{:home} #{:home} [])))
    (is (solved? #{:home}
                 (gps #{:home} #{:home} [])))
    (is (= #{:son-at-school :car-works}
           (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                #{:son-at-school}
                school-ops)))
    (is (solved? #{:son-at-school :car-works}
                 (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                      #{:son-at-school}
                      school-ops))))
  (testing "unsolvable problem"
    (is (= #{:son-at-home :car-needs-battery :have-money}
           (gps #{:son-at-home :car-needs-battery :have-money}
                #{:son-at-school}
                school-ops)))
    (is (not (solved? #{:son-at-school}
                      (gps #{:son-at-home :car-needs-battery :have-money}
                           #{:son-at-school}
                           school-ops))))))
