;;; pq.el --- libpq binding  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Tom Gillespie <tgbugs@gmail.com>
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

(require 'pq-core nil t) ; Don't signal an error if the module is absent.

;; Try and compile the `pq-core' module when we compile the PQ package.
(eval-when-compile
  (unless (or (featurep 'pq-core)
              ;; Try and avoid running this code when we're just
              ;; loading the uncompiled `pq.el'.
              (and (fboundp 'macroexp-compiling-p) ;Emacs≥28
                   (not (macroexp-compiling-p))))
    (require 'pq-compile)
    (declare-function pq--compile-module "pq-compile" ())
    (ignore-errors (pq--compile-module))))

;; Try and compile the `pq-core' module when the PQ package is loaded.
(unless (featurep 'pq-core)
  (require 'pq-compile)
  (declare-function pq--compile-maybe "pq-compile" ())
  (pq--compile-maybe))


(provide 'pq)

;;; pq.el ends here
