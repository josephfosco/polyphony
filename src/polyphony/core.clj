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

(ns polyphony.core
  (:require
   [polyphony.node-tree :refer [reset-node-tree]]
   [polyphony.reader :refer [add-rule-to-graph]]
   [polyphony.variables :refer [get-variable set-variable]]
   [polyphony.version :refer [POLYPHONY-VERSION-STR]]
   )
  )

(def reset-num (atom 0))

(defmacro defrule
  [cond-clauses rslt-clauses]
  (add-rule-to-graph cond-clauses rslt-clauses)
  nil
  )

(defmacro set-var
  [var-name var-val]
  `(set-variable '~var-name ~var-val (deref reset-num))
  )

(defn reset-variable-vals
  []
  (reset! reset-num (inc @reset-num))
  (reset-node-tree)
  )

(defmacro get-variable-val
  [var-name]
  `(get-variable '~var-name)
  )

(println)
(println "POLYPHONY rule library version: " POLYPHONY-VERSION-STR)
(println)
