;;; bigquery-mode.el --- Trying to implement a BigQuery major mode

;; Copyright (C) 2020 Christoph Stockhusen

;; Author: Christoph Stockhusen <mail@christophstockhusen.de>
;; Keywords: bigquery
;; Version 0.0.1

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

;; foo

;;; Code:

;; code goes here

(defvar bigquery-mode-hook nil)

(defvar bigquery-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") 'bigquery-dry-run-query)
    (define-key map (kbd "C-c e") 'bigquery-run-query)
    map)
  "Keymap for BigQuery major mode")

(defun bigquery-dry-run-query ()
  (bigquery-set-current-project-id))

(defun bigquery-run-query ()
  (bigquery-set-current-project-id))

(add-to-list 'auto-mode-alist '("\\.sql\\'" . bigquery-mode))

(defconst bigquery-keywords
'("ALL" "AND" "ANY" "ARRAY" "AS" "ASC" "ASSERT_ROWS_MODIFIED" "AT" "BETWEEN" "BY" "CASE" "CAST" "COLLATE" "CONTAINS" "CREATE" "CROSS" "CUBE" "CURRENT" "DEFAULT" "DEFINE" "DESC" "DISTINCT" "ELSE" "END" "ENUM" "ESCAPE" "EXCEPT" "EXCLUDE" "EXISTS" "EXTRACT" "FALSE" "FETCH" "FOLLOWING" "FOR" "FROM" "FULL" "GROUP" "GROUPING" "GROUPS" "HASH" "HAVING" "IF" "IGNORE" "IN" "INNER" "INTERSECT" "INTERVAL" "INTO" "IS" "JOIN" "LATERAL" "LEFT" "LIKE" "LIMIT" "LOOKUP" "MERGE" "NATURAL" "NEW" "NO" "NOT" "NULL" "NULLS" "OF" "ON" "OR" "ORDER" "OUTER" "OVER" "PARTITION" "PRECEDING" "PROTO" "RANGE" "RECURSIVE" "RESPECT" "RIGHT" "ROLLUP" "ROWS" "SELECT" "SET" "SOME" "STRUCT" "TABLESAMPLE" "THEN" "TO" "TREAT" "TRUE" "UNBOUNDED" "UNION" "UNNEST" "USING" "WHEN" "WHERE" "WINDOW" "WITH" "WITHIN"))

(defconst bigquery-function-names '("MAX" "MIN"))

(eval-when-compile
  (defvar bigquery-font-lock-keywords))

(eval-and-compile
  (defun bigquery-font-lock-keyword-builder (face keywords)
    (cons (concat "\\<" (regexp-opt keywords) "\\>") face)))

(eval-when-compile
  (setq bigquery-font-lock-keywords
        (list
         (bigquery-font-lock-keyword-builder 'font-lock-keyword-face bigquery-keywords)
         (bigquery-font-lock-keyword-builder 'font-lock-function-name-face bigquery-function-names))))

(defvar bigquery-font-lock-keywords
  (eval-when-compile bigquery-font-lock-keywords))

(defun bigquery-mode-indent-line ()
  "Indent current line"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (indent-line-to 10)))

(defvar bigquery-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for bigquery-mode")

(defvar bigquery-project-id
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "gcloud config get-value project")))

(defun bigquery-set-current-project-id ()
  (setq mode-name (format "BigQuery[%s]" bigquery-project-id)))

(defun bigquery-mode ()
  "Major mode for editing bigquery scripts"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bigquery-mode-syntax-table)
  (use-local-map bigquery-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bigquery-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'bigquery-indent-line)
  (setq major-mode 'bigquery-mode)
  (bigquery-set-current-project-id)
  (run-hooks 'bigquery-mode-hook)
  )

(provide 'bigquery-mode)

;;; bigquery-mode.el ends here
