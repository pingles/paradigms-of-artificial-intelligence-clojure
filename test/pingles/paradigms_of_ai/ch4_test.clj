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
    (is (= #{:standing '(:executing :stand)}
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
    (is (= #{:standing :stretch '(:executing :stretching) '(:executing :stand)}
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
    (is (= #{:paid '(:executing :pay)}
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
                           school-ops)))))
  (testing "pre-requisite clobbers sibling goal"
    (is (solved? #{:have-money :son-at-school}
                 (gps #{:son-at-home :have-money :car-works}
                      #{:have-money :son-at-school}
                      school-ops)))

    ;; we can't :have-money because we'll need to spend it on a
    ;; battery for the car
    (is (not (solved? #{:have-money :son-at-school}
                      (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                           #{:have-money :son-at-school}
                           school-ops)))))

  (testing "leaping before you look"
    ;; we correctly can't solve the problem, but the initial algorithm
    ;; executes operations that will ultimately cause it to fail
    (is (not (solved? #{:son-at-school :have-money}
                      (gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
                           #{:son-at-school :have-money}
                           school-ops)))))

  (testing "recursive subgoal"
    (let [ops (conj school-ops (make-op :ask-phone-number
                                        :preconditions #{:in-communication-with-shop}
                                        :add-list [:know-phone-number]))]
      (is (not (solved? #{:son-at-school}
                        (gps #{:son-at-home :car-needs-battery :have-money}
                             #{:son-at-school}
                             ops)))))))

