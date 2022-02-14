;;; pq.el --- libpq binding

;; Copyright (C) 2020 by Tom Gillespie

;; Author: Tom Gillespie
;; URL: https://github.com/anse1/emacs-libpq
;; Version: 0.01
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An almost completely empty wrapper to simplify packaging on *elpa.

;;; Code:

(if t (require 'pq-core))

(provide 'pq)

;;; pq.el ends here
