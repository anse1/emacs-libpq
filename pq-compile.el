;;; pq-compile.el --- Compile the `pq-core` module   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar pq--compile-directory
  (file-name-directory
   (or
    (if (fboundp 'macroexp-file-name) (macroexp-file-name)) ;Emacs≥28
    load-file-name
    buffer-file-name
    (locate-library "pq"))))

(defun pq--compile-module ()
  "Compile the `pq-core' module."
  (with-temp-buffer
    (setq default-directory pq--compile-directory)
    (let* ((exitcode (call-process "make" nil t)))
      (if (zerop exitcode)
          (message "Compilation of `pq-core' succeeded")
        (let ((out (buffer-string)))
          (if noninteractive
              (message "Compilation of `pq-core' failed:\n%s" out)
            (with-current-buffer (get-buffer-create "*pq-compile*")
              (setq default-directory pq--compile-directory)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert out))
              (compilation-mode)
              (pop-to-buffer (current-buffer))
              (error "Compilation of `pq-core' failed"))))))))

(defun pq--compile-maybe ()
  (cond
   (noninteractive ;; Batch use: try to compile but delay errors when possible.
    (ignore-errors (pq--compile-module))
    (require 'pq-core nil t))
   ;; FIXME: Should we first try it silently (i.e. without prompting the user)?
   ((not (y-or-n-p "PQ needs to compile the `pq-core' module.  Do it now?"))
    (message "Continuing without `pq-core'; some operations may fail"))
   (t
    (pq--compile-module)
    (require 'pq-core))))


(provide 'pq-compile)
;;; pq-compile.el ends here
