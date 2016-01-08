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

(ns polyphony.variables)

(def all-variables (atom {}))

(defn- new-variable
  [cur-variables variable-name clause-id]
  (assoc cur-variables
    (keyword (name variable-name))
    (conj ((keyword (name variable-name)) cur-variables) (keyword clause-id)))
  )

(defn add-variable
  [variable-name clause-id]
  (swap! all-variables new-variable variable-name clause-id)
  variable-name
  )

(defn get-variable
  [variable-name]
  nil
  )
