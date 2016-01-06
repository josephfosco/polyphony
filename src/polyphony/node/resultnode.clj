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

(ns polyphony.node.resultnode)

(defrecord ResultNode [id input-id input-status result-clauses variables])

(defn create-result-node
  "Used to create a new result-node"
  [input-id rslt-clauses]
  (ResultNode. (gensym 'R_) input-id false rslt-clauses '())
  )
