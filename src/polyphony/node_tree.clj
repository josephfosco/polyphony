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

(ns polyphony.node-tree
  (:require
   [polyphony.node.condnode  :refer  [set-output-for-cond]]
   )
  )

(def all-conds (atom {}))
(def all-joins (atom {}))

(defn add-cond
  [new-cond]
  (swap! all-conds assoc (keyword  (:id new-cond)) new-cond)
  )

(defn add-join
  [new-join]
  (swap! all-joins assoc (keyword  (:id new-join)) new-join)
  )

(defn find-id-for-clause
  [clause]
  (let [id-and-clause (first (for [cond-node (vals @all-conds)
                        :when (= clause (:cond-clause cond-node))]
                    (list (:id cond-node) clause)))]
    id-and-clause)
  )

(defn- set-cn-output
  [cur-cond-nodes cond-node-id output-id]
  (assoc cur-cond-nodes
    (keyword cond-node-id)
    (set-output-for-cond ((keyword cond-node-id) cur-cond-nodes) output-id))
  )

(defn set-cond-node-output
  [cond-node-id output-id]
  (swap! all-conds set-cn-output cond-node-id output-id)
  )
