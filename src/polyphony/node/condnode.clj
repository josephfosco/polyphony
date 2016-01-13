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

(ns polyphony.node.condnode
  (:require
   [polyphony.utils :refer [is-variable?]]
   )
  )

(defrecord CondNode [id cond-clause num-variables variables outputs])

(defn create-cond-node
  "Used to create a new cond-node

   id-and-clause - a list with the first element the id for the
                   clause, and the second element the clause"
  [id-and-clause]
  (CondNode. (first id-and-clause) (second id-and-clause) nil '{} '())
  )

(defn- eval-cond-node
  "replace all variables in cond-clause with their value and
     eval the resulting expression"
  [cond-node]
  (println "eval-cond-node: " cond-node)
  (eval
   (for [elem (:cond-clause cond-node)]
     (if (is-variable? elem)
       ((keyword (name elem)) (:variables cond-node))
       elem
       )
     ))
  )

(defn set-cond-output
  [cond-node output-id]
  (assoc cond-node :outputs (conj (:outputs cond-node) output-id ))
  )

(defn set-cond-num-variables
  [cond-node vars]
  (assoc cond-node :num-variables (count vars))
  )

(defn set-cond-variable
  [cond-node var-name var-val]
  (println "set-cond-variable: " var-name var-val)
  (let [new-cond-node (assoc cond-node
                        :variables
                        (assoc (:variables cond-node)
                          (keyword var-name) var-val))
        ]
    (if (= (count (:variables new-cond-node)) (:num-variables new-cond-node))
      (println (eval-cond-node new-cond-node))
      )
    new-cond-node
    )
  )
