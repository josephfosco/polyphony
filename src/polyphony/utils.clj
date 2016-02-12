;    Copyright (C) 2016  Joseph Fosco. All Rights Reserved
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

(ns polyphony.utils)

(defn is-variable?
  [var-name]
  (and (= (type var-name) clojure.lang.Symbol)
       (= \? (first (name var-name))))
  )

(defn sym-to-key
  [sym]
  (keyword (name sym))
  )

(defn substitute-variable-vals
  [clause variable-dict]
  (doall (for [elem clause]
           (if (is-variable? elem)
             ((keyword (name elem)) variable-dict)
             elem
             )
           ))
  )

(declare substitute-result-variable-vals)
(defn subst-var
  [elem variable-dict clause-vec ndx]
  (cond (and  (is-variable? elem) (not (.endsWith (name (get clause-vec
                                                             (dec ndx)))
                                                  "set-var")))
        ((keyword (name elem)) variable-dict)
        (seq? elem)
        (substitute-result-variable-vals elem variable-dict)
        :else
        elem
    )
  )

(defn substitute-result-variable-vals
  [clause variable-dict]
  (map subst-var clause (repeat variable-dict) (repeat (vec clause)) (range))
  )
