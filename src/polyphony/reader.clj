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
   [polyphony.node-tree :refer [add-cond find-id-for-clause]]
   [polyphony.variables :refer [add-variable]]
   )
  )

(defn create-variables
  [clause-id clause]
  (println "create_variables")
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

(defmacro defrule
  [cond-clause rslt-clause]
  `(let [cond-ids# '~(for [clause cond-clause] (list (gensym 'C_) clause))]
     (dorun (map add-cond (map create-cond-node
                               (map first cond-ids#)
                               (map second cond-ids#))))
     (dorun (map create-variables (map first cond-ids#) (map second cond-ids#)))
     (dorun (map find-id-for-clause (map second cond-ids#)))
     )
  )
