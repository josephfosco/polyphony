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

(ns polyphony.node-tree
  (:require
   [polyphony.node.condnode :refer [set-cond-output set-cond-num-variables
                                    set-cond-variable]]
   [polyphony.node.joinnode :refer [set-join-output set-join-right-input]]
   [polyphony.utils :refer [sym-to-key]]
   )
  )

(def all-conds (atom {}))
(def all-joins (atom {}))
(def all-results (atom {}))

(defn add-cond
  [new-cond-as-atom]
  (swap! all-conds assoc (keyword  (:id @new-cond-as-atom)) new-cond-as-atom)
  )

(defn get-cond-node
  [cond-id]
  ((sym-to-key cond-id) @all-conds)
  )

(defn add-join
  [new-join-as-atom]
  (swap! all-joins assoc (keyword  (:id @new-join-as-atom)) new-join-as-atom)
  )

(defn add-result
  [new-result]
  (swap! all-results assoc (keyword  (:id new-result)) new-result)
  )

(defn find-id-for-clause
  [clause]
  (let [id-and-clause (first (for [cond-node (map deref (vals @all-conds))
                        :when (= clause (:cond-clause cond-node))]
                    (list (:id cond-node) clause)))]
    id-and-clause)
  )

(defn- set-cn-output
  [cur-cond-nodes cond-node-id output-id]
  (assoc cur-cond-nodes
    (keyword cond-node-id)
    (set-cond-output ((keyword cond-node-id) cur-cond-nodes) output-id))
  )

(defn set-cond-node-output
  [cond-node-id output-id]
  (swap! all-conds set-cn-output cond-node-id output-id)
  )

(defn- set-cn-variable
  [cur-cond-nodes cond-node-id-key variable-name variable-val]
  (assoc cur-cond-nodes
    cond-node-id-key
    (set-cond-variable (cond-node-id-key cur-cond-nodes)
                        variable-name
                        variable-val))
  )

(defn set-cond-node-variable
  [cond-node-id-key variable-name variable-val]
  (println "set-cond-node-variable: " cond-node-id-key)
  (swap! all-conds set-cn-variable cond-node-id-key variable-name variable-val)
  )

(defn- set-jn-output
  [cur-join-nodes join-node-id output-id]
  (assoc cur-join-nodes
    (keyword join-node-id)
    (set-join-output ((keyword join-node-id) cur-join-nodes) output-id))
  )

(defn set-join-node-output
  [join-node-id output-id]
  (swap! all-joins set-jn-output join-node-id output-id)
  )
