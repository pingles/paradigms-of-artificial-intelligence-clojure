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

(deftest general-problem-solver
  (is (= :solved
         (gps #{:home} #{:home} []))))
