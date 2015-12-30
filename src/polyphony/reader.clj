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
   [polyphony.node-tree :refer [add-cond find-id-for-clause add-join set-cond-node-output]]
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
  (let [new-join (when (> (count clauses) 1)
                   (create-join-node (first (first clauses))))]
    (cond (and (nil? join-node) new-join)
          (do
            (add-join new-join)
            (set-cond-node-output (first (first clauses)) (:id new-join))
            (recur new-join (rest clauses))
            )
          )
    )
  )

(defn- graph-cond-clauses
  [cond-clauses]

  (if (= (count cond-clauses) 1)
    (first cond-clauses)
    (create-joins nil cond-clauses)
    )

  (comment
    if 1 clause and no join clause return clause
    if 1 clause and join cluse and make clause input of join clause
        return join clause
    if no join clause (**first time**)  then add join clause with
        first clause as input
        remove first clause loop rest of clauses and join clause
    join first clause with join clause create new join clause woth
        old join clause as input
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
