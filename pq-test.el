;;; pq-test.el --- test libpq bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

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

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))

(require 'pq)
(require 'ert)

(defvar *conninfo* (or (getenv "PG_CONNINFO") "port=5432 dbname=smith"))

(ert-deftest pq-query-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (should (equal (pq:query conn "select 1 union select 2") '(1 2)))
    (should (equal (pq:query conn "select 1,2") '([1 2])))
    (should (equal (pq:query conn "select 1,2 union select 3,4") '([1 2] [3 4])))
    (should (equal (pq:query conn "select 'Hello, ' || $1::text" (user-login-name))
                   (list (concat "Hello, " (user-login-name)))))

    (should (equal (pq:query conn "select true, false, NULL, 42")
                   '([t nil nil 42])))

    ;; Multiple statements
    (should (equal (pq:query conn "select 1; select 2; select 3;") '(3)))))

(ert-deftest pq-encoding-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (skip-unless (equal '("UTF8") (pq:query conn "show server_encoding")))
    (should (equal (pq:query conn "select length('(╯°□°)╯︵ ┻━┻')")
                   '(12)))
    (should (equal (pq:query conn "select '(╯°□°)╯' ||$1::text" "︵ ┻━┻")
                   '("(╯°□°)╯︵ ┻━┻")))
    (should-error (pq:query conn "select $1::text" "\x80"))
    (should-error (pq:query conn "select text '\x80'"))))

(ert-deftest pq-escape-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (should-error (pq:escapeLiteral "d"))
    (should-error (pq:escapeIdentifier "d"))
    (should-error (pq:escapeLiteral "'" "d"))
    (should-error (pq:escapeIdentifier "'" "d"))
    (should (equal (pq:escapeLiteral conn "mo'oo\"oo") "'mo''oo\"oo'"))
    (should (equal (pq:escapeIdentifier conn "moo'oo\"oo") "\"moo'oo\"\"oo\""))))

(ert-deftest pq-garbage-collect-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (garbage-collect)
    (sleep-for 0.2)
    (setq conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*))
    (let ((connection-count
	   (car
	    (pq:query
	     conn
	     "select count(1) from pg_stat_activity where application_name = 'emacs'"))))
      (garbage-collect)
      (sleep-for 0.2)
      (should
       (<
	(car
	 (pq:query
	  conn
	  "select count(1) from pg_stat_activity where application_name = 'emacs'"))
	(- connection-count 6))))))

(ert-deftest pq-signal-error-test ()
  (should-error (pq:connectdb "invalid-conninfo"))
  (let ((conn (pq:connectdb *conninfo*)))
    (pq:query conn "select 1")
    (should-error (pq:query "select * from"))
    (should-error (pq:query conn "select * from"))
    (should-error (pq:query conn "select $1::text"))
    (should
     (equal
      'ok
      (condition-case err
	  (pq:query conn "moo")
	(pq:error
	 (if (string= "42601" (nth 2 err))
	     ;; syntax errors are ok
	     'ok
	   (signal (car err) (cdr err)))))))
))

(ert-deftest pq-reset-connection-test ()
  (let ((testconn (pq:connectdb *conninfo*))
	(ctlconn (pq:connectdb *conninfo*)))
    (let ((victim (car (pq:query testconn "select pg_backend_pid()"))))
      (pq:query ctlconn "select pg_terminate_backend($1)" victim))
    (should-error (pq:query testconn "select 1"))
    (pq:reset testconn)
    (pq:query testconn "select 1")))

(ert-deftest pq-notice-receiver-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (pq:query conn "set client_min_messages to notice")
    (pq:query conn "drop table if exists ert_nonexisting_table")
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (re-search-backward "ert_nonexisting_table"))))

(ert-deftest pq-async-notify-test ()
  (let* ((conn (pq:connectdb *conninfo*))
	 (mypid (car (pq:query conn "select pg_backend_pid()"))))
    (pq:notifies conn)
    (pq:query conn "listen ert_notification")
    (pq:query conn "notify ert_notification")
    (should
     (equal
      (car (pq:notifies conn))
      (vector "ert_notification" mypid "")))
    (should (equal (pq:notifies conn) nil))
    (pq:query conn "notify ert_notification, 'paylöad'")
    (should
     (equal
      (car (pq:notifies conn))
      (vector "ert_notification" mypid "paylöad")))
    (should-error (pq:notifies 0))
    (should-error (pq:notifies nil))
    (should-error (pq:query conn "select pg_terminate_backend($1)" mypid))
    (should-error (pq:notifies conn))
))

;;; pq-test.el ends here
