;    Copyright (C) 2015-2016  Joseph Fosco. All Rights Reserved
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
   [polyphony.node.condnode :refer [create-cond-node
                                    set-cond-num-variables set-cond-output]]
   [polyphony.node.joinnode :refer [create-join-node set-join-right-input-id
                                    set-join-output-node]]
   [polyphony.node-tree :refer [add-cond find-id-for-clause add-join
                                get-cond-node add-result]]
   [polyphony.node.resultnode :refer [create-result-node]]
   [polyphony.utils :refer [is-variable?]]
   [polyphony.variables :refer [add-variable]]
   )
  )

(defn create-cond-variables
  [cond-node-as-atom]

  (list cond-node-as-atom
        (for [tstvar (:cond-clause @cond-node-as-atom)
              :when (is-variable? tstvar)]
     (add-variable tstvar cond-node-as-atom)
     )
   )
  )

(declare find-result-variables)
(defn add-result-vars
  [elem result-node-as-atom clause-vec ndx ]
  (cond (and  (is-variable? elem) (not (.endsWith (name (get clause-vec
                                                             (dec ndx)))
                                                  "set-var")))
        (do
          (println "adding result var: " elem)
          (add-variable elem result-node-as-atom)
          )
        (seq? elem)
        (find-result-variables elem result-node-as-atom)
    )
  )

(defn find-result-variables
  [clause result-node-as-atom]
  (println "find-result-variables clause: " clause)
  (dorun (map add-result-vars
              clause
              (repeat result-node-as-atom)
              (repeat (vec clause))
              (range)))
  )


(defn create-result-variables
  [result-node-as-atom]
  (println)
  (println "create-result-variables " (:result-clauses @result-node-as-atom))
  (dorun (for [result-clause (:result-clauses @result-node-as-atom)]
           (find-result-variables result-clause result-node-as-atom)))
  )

(defn add-num-variables-to-cond
  "cond-var-list - a list containing a cond-node as an atom and
   a list of vars in the cond"
  [cond-var-list]
  (println "add-variables-to-cond: " cond-var-list)
  (reset! (first cond-var-list)
          (set-cond-num-variables
            (deref (first cond-var-list))
            (second cond-var-list)))
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
  [join-node-as-atom cond-nodes-as-atoms]
  (println)
  (println "create-joins " join-node-as-atom cond-nodes-as-atoms)
  (println)
  (let [new-join (when (seq cond-nodes-as-atoms)
                   (atom (create-join-node
                          (:id (deref (first cond-nodes-as-atoms))))))]
    (cond (nil? new-join)
          ;; no more clauses, return last join
          join-node-as-atom
          (and (nil? join-node-as-atom) new-join)
          ;; first time add first 2 clauses to join
          (do
            (add-join new-join)
            (reset! new-join
                    (set-join-right-input-id
                     @new-join
                     (:id (deref (second cond-nodes-as-atoms)))))
            (reset! (first cond-nodes-as-atoms)
                    (set-cond-output (deref (first cond-nodes-as-atoms))
                                     new-join))
            (reset! (second cond-nodes-as-atoms)
                    (set-cond-output (deref (second cond-nodes-as-atoms))
                                     new-join))
            (recur new-join (rest (rest cond-nodes-as-atoms)))
            )
          :else
          ;; add join-node and first clause to new-join
          (do
            (add-join new-join)
            (reset! (first cond-nodes-as-atoms)
                    (set-cond-output (deref (first cond-nodes-as-atoms))
                                     new-join))
            (reset! new-join
                    (set-join-right-input-id @new-join
                                             (:id @join-node-as-atom)))
            (reset! join-node-as-atom (set-join-output-node @join-node-as-atom
                                                            new-join))
            (recur new-join (rest cond-nodes-as-atoms)))
          )
    )
  )

(defn- graph-cond-clauses
  [cond-nodes-as-atoms]
  (if (= (count cond-nodes-as-atoms) 1)
    (first cond-nodes-as-atoms)
    (create-joins nil cond-nodes-as-atoms)
    )
  )

(defn- graph-result-clauses
  [rslt-clauses input-clause-atom]
  (let [rslt (atom (create-result-node (:id @input-clause-atom) rslt-clauses))]
    (add-result rslt)
    (cond (.startsWith (name (:id @input-clause-atom)) "C")
          (reset! input-clause-atom (set-cond-output @input-clause-atom rslt))
          (.startsWith (name (:id @input-clause-atom)) "J")
          (reset! input-clause-atom (set-join-output-node @input-clause-atom rslt))
          :else
          (throw (Throwable. "InvalidNodeId"))
          )
    rslt
    )
  )

(defn add-rule-to-graph
  [cond-clauses rslt-clauses]
  (let [existing-conds (get-existing-conds cond-clauses)
        new-conds (new-clause cond-clauses)
        existing-cond-nodes (map get-cond-node (map first existing-conds))
        new-cond-nodes (map atom (map create-cond-node new-conds))
         ]

    (println)
    (println "existing-conds: " existing-conds)
    (println "new-conds: " new-conds)
    (println "existing-cond-nodes: " existing-cond-nodes)
    (println "new-cond-nodes: " new-cond-nodes)
    (println)

    (dorun (map add-cond new-cond-nodes))
    (dorun (map add-num-variables-to-cond
                (map create-cond-variables new-cond-nodes))
           )
    (->> new-cond-nodes
         (into existing-cond-nodes)
         (graph-cond-clauses)
         (graph-result-clauses rslt-clauses)
         (create-result-variables)
         )
    )
  )
