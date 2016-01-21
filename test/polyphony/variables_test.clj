(ns polyphony.variables-test
  (:use clojure.test
        polyphony.variables))

(deftest test-add-variable
  (testing "add one variable"
    (add-variable '?var1 (atom {:id 'node1}))
    (println ((keyword (name '?var1)) @all-variables))
    (= 1 1)
    ;;(is (= 'node1 (:id (deref (first @all-variables)))))
    ))
