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

    ;; provoke a warning
    ;; (pq:query con "drop table if exists nonexisting_table")

    ;; Multiple statements
    (should (equal (pq:query conn "select 1; select 2; select 3;") '(3)))))

(ert-deftest pq-escape-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (should (equal (pq:escapeLiteral conn "mo'oo\"oo") "'mo''oo\"oo'"))
    (should (equal (pq:escapeIdentifier conn "moo'oo\"oo") "\"moo'oo\"\"oo\""))))

(ert-deftest pq-garbage-collect-test ()
  (let ((conn (pq:connectdb *conninfo*)))
    (garbage-collect)
    (setq conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*)
	  conn (pq:connectdb *conninfo*))
    (should
     (equal
      (pq:query conn
		"select count(1) > 5 from pg_stat_activity where application_name = 'emacs'")
      '(t)))
    (garbage-collect)
    (should
     (equal
      (pq:query conn
		"select count(1) < 5 from pg_stat_activity where application_name = 'emacs'")
      '(t)))))

(ert-deftest pq-signal-error-test ()
  (should-error (pq:connectdb "invalid-conninfo")))
