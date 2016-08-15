(add-to-list 'load-path
             (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))

(require 'pq)

(setq con (pq:connectdb "port=5432 dbname=smith"))
(pq:query con "select version()")
;; ("PostgreSQL 9.4.8 on i686-pc-linux-gnu, compiled by gcc (Debian 4.9.2-10) 4.9.2, 32-bit")
(pq:query con "select 1 union select 2")
;; (1 2)
(pq:query con "select 1,2")
;; ([1 2])
(pq:query con "select 1,2 union select 3,4")
;; ([1 2] [3 4])
(pq:query con "select 'Hello, ' || $1::text" (user-login-name))
;; ("Hello, andreas")
(pq:escapeLiteral con "mo'oo\"oo")
;; "'mo''oo\"oo'"
(pq:escapeIdentifier con "moo'oo\"oo")
;; "\"moo'oo\"\"oo\""
(pq:query con "select true, false, NULL, version()")

;; provoke a warning
(pq:query con "drop table if exists nonexisting_table")

;; Multiple statements
(pq:query con "select 1; select 2; select 3;")

(setq con (pq:connectdb))
(setq con nil)
(garbage-collect)
