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

(ns polyphony.node.condnode)

(defrecord CondNode [id cond-clause num-variables variables outputs])

(defn create-cond-node
  "Used to create a new cond-node

   id-and-clause - a list with the first element the id for the
                   clause, and the second element the clause"
  [id-and-clause]
  (CondNode. (first id-and-clause) (second id-and-clause) nil '{} '())
  )

(defn set-cond-output
  [cond-node output-id]
  (assoc cond-node :outputs (conj (:outputs cond-node) output-id ))
  )

(defn set-cond-num-variables
  [cond-node vars]
  (assoc cond-node :num-variables (count vars))
  )
