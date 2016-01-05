;    Copyright (C) 2015  Joseph Fosco. All Rights Reserved
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

(ns polyphony.reader
  (:require
   [polyphony.node.condnode :refer [create-cond-node]]
   [polyphony.node.joinnode :refer [create-join-node set-output]]
   [polyphony.node-tree :refer [add-cond find-id-for-clause add-join set-cond-node-output
                                set-join-node-right-input]]
   [polyphony.variables :refer [add-variable]]
   )
  )

(defn create-variables
  [clause-id clause]
  (println clause-id clause)

  (dorun
   (for [tstvar clause :when (and (= (type tstvar) clojure.lang.Symbol)
                                  (= \? (first (name tstvar))))]
     (do
       (add-variable tstvar clause-id)
       (println (name tstvar) " is a variable")
       tstvar
       )
     ))
  )

(defn get-existing-conds
  [cond-clauses]
  (filter #(not (nil? %)) (map find-id-for-clause cond-clauses))
  )

(defn new-clause
  [clauses]
  (filter #(not (nil? %))
          (map  #(when (not (find-id-for-clause %))
                   (list (gensym 'C_) %)
                   )
                clauses))
  )
(defn- create-joins
  [join-node clauses]
  (println "create-joins " join-node clauses)
  (let [new-join (when (seq clauses)
                   (create-join-node (ffirst clauses)))]
    (cond (nil? new-join)
          ;; no more clauses, return last join
          join-node
          (and (nil? join-node) new-join)
          ;; first time add first 2 clauses to join
          (do
            (add-join new-join)
            (set-join-node-right-input (:id new-join) (first (second clauses)))
            (set-cond-node-output (ffirst clauses) (:id new-join))
            (recur new-join (rest (rest clauses)))
            )
          :else
          ;; add join-node and first clause to new-join
          (do
            (add-join new-join)
            (set-cond-node-output (ffirst clauses) (:id new-join))
            (set-join-node-right-input (:id new-join) (:id join-node))
            (recur new-join (rest clauses)))
          )
    )
  )

(defn- graph-cond-clauses
  [cond-clauses]
  (if (= (count cond-clauses) 1)
    (first cond-clauses)
    (create-joins nil cond-clauses)
    )
  )

(defmacro defrule
  [cond-clauses rslt-clauses]
  (let [existing-conds (get-existing-conds cond-clauses)
         new-conds (new-clause cond-clauses)
         ]

    (println)
    (println "existing-conds: " existing-conds)
    (println "new-conds: " new-conds)
    (println)

    (dorun (map add-cond (map create-cond-node new-conds)))
    (dorun (map create-variables (map first new-conds) (map second new-conds)))
    (graph-cond-clauses (into existing-conds new-conds))


     )
  )
