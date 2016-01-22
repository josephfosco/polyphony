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
  (:require [ polyphony.utils :refer [sym-to-key]])
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
  [f]
  (defrule ((= ?var1 10)) (()))
  (defrule ((= ?var2 10) (= ?var3 10)) (()))
  (defrule ((= ?var4 10) (= ?var5 10) (= ?var6 10)) (()))
  (defrule ((= ?var7 10)) (()))
  (defrule ((= ?var7 10)) (()))
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

(defn get-result-ids-for-cond-id
  "Returns a list of result-ids for cond-id"
  [cond-id]
  (doall (for [output (:outputs
                       (deref ((sym-to-key cond-id)
                               @polyphony.node-tree/all-conds)))]
           (:id @output)
           ))
  )

(defn get-result-node
  [result-id]
  (deref ((sym-to-key result-id) @polyphony.node-tree/all-results))
  )

(deftest test-single-var-rule-to-result
  (let [cond-id (get-cond-id-for-var '?var1)
        result-id (first (get-result-ids-for-cond-id cond-id))
        ]
    (testing "single variable rule connects to result"
        (is (= (:input-id (get-result-node result-id)) cond-id))
      )
    ))

(deftest test-cond-node-has-multiple-outputs
  (let [cond-id (get-cond-id-for-var '?var7)
        result-ids (get-result-ids-for-cond-id cond-id)
        ]
    (testing "cond node has multiple outputs"
      (is (= (count result-ids) 2))
      (is (= (:input-id (get-result-node (first result-ids)))
             (:input-id (get-result-node (second result-ids)))
             cond-id
             ))
      )
    ))
