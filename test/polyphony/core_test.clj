;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns polyphony.core-test
  (:use clojure.test
        polyphony.core
        )
  (:require [polyphony.utils :refer [sym-to-key]])
  )

(defn clear-polyphony
  []
  (reset! polyphony.node-tree/all-conds {})
  (reset! polyphony.node-tree/all-joins {})
  (reset! polyphony.node-tree/all-results {})
  (reset! polyphony.variables/all-variables {})
  )

(clear-polyphony)

(defn rule-fixture
  "For testing ?varx should never be set"
  [f]
  (defrule ((= ?var1 10)) (()))
  (defrule ((= ?var2 10) (= ?var3 10)) (()))
  (defrule ((= ?var4 10) (= ?var5 10) (= ?var6 10)) (()))
  (defrule ((= ?var7 10)) (()))
  (defrule ((= ?var7 10)) (()))
  (defrule ((= ?var70 10) (?= varx 100)) (()))
  (defrule ((= ?varx 100)) ((set-var ?var71 10)))
  (defrule ((= ?var72 10)) ((set-var ?var72 11)))
  (defrule ((= ?var73 10) (= ?var74 10)) ((set-var ?var73 (+ ?var74 1))))
  (f)
  )

(use-fixtures :once rule-fixture)

(defn get-cond-id-for-var
  "Returns the first cond-id that is assigned to var-id
   most useful when you know the var-id is used in only 1 cond"
  [var-id]
  (:id (deref (first ((keyword (name var-id))
                      @polyphony.variables/all-variables))))
  )

(defn get-output-ids-for-cond-id
  "Returns a list of output-ids for cond-id"
  [cond-id]
  (doall (for [output (:outputs
                       (deref ((sym-to-key cond-id)
                               @polyphony.node-tree/all-conds)))]
           (:id @output)
           ))
  )

(defn get-join-node
  [join-id]
  (deref ((sym-to-key join-id) @polyphony.node-tree/all-joins))
  )

(defn get-result-node
  [result-id]
  (deref ((sym-to-key result-id) @polyphony.node-tree/all-results))
  )

(deftest test-single-var-rule-to-result
  (let [cond-id (get-cond-id-for-var '?var1)
        result-id (first (get-output-ids-for-cond-id cond-id))
        ]
    (testing "single variable rule connects to result"
        (is (= (:input-id (get-result-node result-id)) cond-id))
      )
    ))

(deftest test-two-rules-to-join-and-result
  (let [cond-id-1 (get-cond-id-for-var '?var2)
        cond-id-2 (get-cond-id-for-var '?var3)
        output-id-1 (first (get-output-ids-for-cond-id cond-id-1))
        output-id-2 (first (get-output-ids-for-cond-id cond-id-2))
        left-input-id (:left-input-id (get-join-node output-id-1))
        right-input-id (:right-input-id (get-join-node output-id-2))

        ]
    (testing "two rules connect to join and result"
      ;; both conds should connect to same join
      (is (= output-id-1 output-id-2))
      ;; input of join should be the 2 conds
      (is (or (= cond-id-1 left-input-id) (= cond-id-1 right-input-id)))
      (is (or (= cond-id-2 left-input-id) (= cond-id-2 right-input-id)))
      ;; join should output to result node and result node should connect to join
      (is (= (:input-id (deref (:output-node (get-join-node output-id-1))))
             output-id-1))
      )
    )
  )

(deftest test-cond-node-has-multiple-outputs
  (let [cond-id (get-cond-id-for-var '?var7)
        result-ids (get-output-ids-for-cond-id cond-id)
        ]
    (testing "cond node has multiple outputs"
      (is (= (count result-ids) 2))
      (is (= (:input-id (get-result-node (first result-ids)))
             (:input-id (get-result-node (second result-ids)))
             cond-id
             ))
      )
    ))

(deftest test-get-variable-val
  (testing "get-variable-val returns correct value in cond node"
    (set-var ?var70 20)
    (is (= (get-variable-val ?var70) 20))
    )
  (testing "get-variable-val returns correct value in result node"
    (set-var ?var71 20)
    (is (= (get-variable-val ?var71) 20))
    )
  )

(deftest test-set-var-in-resultnode
  (testing "set-var in result node when variable already set"
    (set-var ?var72 10)
    (is (= (get-variable-val ?var72) 11))
    )
    (testing "nested set-var in resultnode"
      (set-var ?var73 10)
      (set-var ?var74 10)
      (is (= (get-variable-val ?var73) 11))
      )
    )
