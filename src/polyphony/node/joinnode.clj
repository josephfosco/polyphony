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

(ns polyphony.node.joinnode
  (:require
   [polyphony.node.resultnode :refer [set-result-atom-input-val]]
   )
  )

(defrecord JoinNode [id left-input-id left-input-status right-input-id
                     right-input-status output-node])

(defn create-join-node
  "Used to create a new join-node"
  [left-input-id]
  (JoinNode. (gensym 'J_) left-input-id false nil false nil)
  )

(defn reset-join-node
  [join-node]
  (assoc join-node :left-input-status false :right-input-status false)
  )

(defn set-join-right-input-id
  [join-node right-input-id]
  (assoc join-node :right-input-id right-input-id :right-input-status false)
  )

(defn set-join-output-node
  [join-node output-node]
  (println join-node output-node)
  (assoc join-node :output-node output-node)
  )

(declare set-join-atom-output-val)
(defn- send-output-val
  [join-node val]
  (println "join send-output-val")
  (cond (.startsWith (name (:id (deref (:output-node join-node)))) "J")
        (set-join-atom-output-val (:output-node join-node)
                                  (:id join-node)
                                  val)
        (.startsWith (name (:id (deref (:output-node join-node)))) "R")
        (do
          (println "join send-output-val: " (:output-node join-node))
          (set-result-atom-input-val (:output-node join-node) val)
          )
        :else
        (throw (Throwable. "InvalidOutputNode"))
        )
  )

(defn set-join-output-val
  [join-node input-id val]
  (println "set-join-output-val: " input-id val)
  (cond (= (:left-input-id join-node) input-id)
        (assoc join-node :left-input-status val)
        (= (:right-input-id join-node) input-id)
        (assoc join-node :right-input-status val)
        :else (throw (Throwable. "InvalidJoinNodeId"))
        )
  )

(defn set-join-atom-output-val
  [join-node-atom input-id val]
  (println "set-join-atom-output-val")
  (let [new-join-node (swap! join-node-atom set-join-output-val input-id val)]
    (when (and (= (:left-input-status new-join-node) true)
               (= (:right-input-status new-join-node) true)
               )
      (send-output-val new-join-node true)
      )
    )
  )
