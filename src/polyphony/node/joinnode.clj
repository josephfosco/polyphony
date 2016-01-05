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

(defrecord JoinNode [id left-input-id left-input-status right-input-id right-input-status output])

(defn create-join-node
  "Used to create a new join-node"
  [left-input-id]
  (println "create-join-node left-input-id:" left-input-id)
  (JoinNode. (gensym 'J_) left-input-id false nil false nil)
  )

(defn set-join-right-input
  [join-node right-input-id]
  (assoc join-node :right-input-id right-input-id :right-input-status false)
  )

(defn set-output
  [output-node-id]
  )
