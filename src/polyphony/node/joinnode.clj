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

(ns polyphony.node.joinnode)

(defrecord JoinNode [id left-input left-input-status right-input right-input-status output-node])

(defn create-join-node
  "Used to create a new join-node"
  [left-input]
  (JoinNode. (gensym 'J_) left-input false nil false nil)
  )

(defn set-join-right-input
  [join-node right-input]
  (assoc join-node :right-input right-input :right-input-status false)
  )

(defn set-join-output-node
  [join-node output-node]
  (println join-node output-node)
  (assoc join-node :output-node output-node)
  )

(defn set-join-output-val
  [join-node input-id val]
  )
