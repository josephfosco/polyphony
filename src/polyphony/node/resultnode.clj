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

(ns polyphony.node.resultnode
  (:require
   [polyphony.utils :refer [substitute-variable-vals]]
   )
  )

(defrecord ResultNode [id input-id input-status result-clauses variables])

(defn create-result-node
  "Used to create a new result-node"
  [input-id rslt-clauses]
  (ResultNode. (gensym 'R_) input-id false rslt-clauses {})
  )

(defn reset-result-node
  [result-node]
  (assoc result-node :variables {} :input-status false)
  )

(defn eval-result-clauses
  [result-node]
  (println "eval-result-clauses: ")
  (dorun (for [clause (:result-clauses result-node)]
           (eval (substitute-variable-vals clause (:variables result-node)))
           )
         )
  )

(defn- set-result-variable
  [result-node var-name var-val]
  (assoc result-node
    :variables
    (assoc (:variables result-node)
      (keyword var-name) var-val))
  )

(defn set-result-atom-variable
  [result-node-atom var-name val]
  (swap! result-node-atom set-result-variable var-name val)
  )

(defn set-result-input-val
  [result-node val]
  (println "set-result-input-val: " val)
  (assoc result-node :input-status val)
  )

(defn set-result-atom-input-val
  [result-node-atom val]
  (println "set-result-atom-input-val: " result-node-atom)
  ;; Only set input status if input-status is not currently true
  ;; In other words - only execute result first time input-status
  ;; is set to true
  (when (not (:input-status @result-node-atom))
    (let [new-result-node (swap! result-node-atom set-result-input-val val)]
      (when (:input-status new-result-node)
        (eval-result-clauses new-result-node))
      )
    )
  )
