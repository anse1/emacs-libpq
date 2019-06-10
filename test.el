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
