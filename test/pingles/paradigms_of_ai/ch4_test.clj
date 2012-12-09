(ns pingles.paradigms-of-ai.ch4-test
  (:use [clojure.test]
        [pingles.paradigms-of-ai.ch4] :reload))

(deftest membership
  (is (member? 1 [1 2 3])))

(deftest general-problem-solver
  (is (= :solved
         (gps #{:home} #{:home} []))))
