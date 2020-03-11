;;; bigquery-mode.el --- Trying to implement a BigQuery major mode

;; Copyright (C) 2020 Christoph Stockhusen

;; Author: Christoph Stockhusen <mail@christophstockhusen.de>
;; Keywords: bigquery
;; Version 0.0.1
;; Package-Requires: ((sql-ident "1.4))

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

(require 'json)
(require 'sql-indent)

(defvar bigquery-mode-hook nil)

(defvar bigquery-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") 'bigquery-dry-run-query)
    (define-key map (kbd "C-c e") 'bigquery-run-query)
    (define-key map (kbd "C-c t") 'bigquery-show-datasets)
    map)
  "Keymap for BigQuery major mode")

(defun bigquery-dry-run-query ()
  (bigquery-set-current-project-id))

(defun bigquery-run-query ()
  (bigquery-set-current-project-id))

(defconst bigquery-datasets-buffer-name "* BigQuery Datasets *")
(defconst bigquery-tables-buffer-name "* BigQuery Tables *")
(defconst bigquery-schema-buffer-name "* BigQuery Schema *")

(defun bigquery-fetch-datasets ()
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string "bq ls --format=json"))))

(defun bqm-tab-list-dataset-button-props (dataset-name)
  (list 'action (lambda (b) (bigquery-show-tables (button-get b 'dataset))) 'dataset dataset-name))

(defun bqm-fetch-tabulated-datasets-list ()
  (let ((datasets (bigquery-fetch-datasets)))
    (mapcar
     (lambda (e) (let ((dataset (cdr (assoc 'id e))))
                   (list nil (vector (cons dataset (bqm-tab-list-dataset-button-props dataset))
                                     (cdr (assoc 'location e))))))
     datasets)))
    
(defun bigquery-fetch-tables (dataset-name)
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string (format "bq ls --format=json %s" dataset-name)))))

(defun bqm-tab-list-table-button-props (table-name)
  (list 'action (lambda (b) (bigquery-show-schema (button-get b 'table))) 'table table-name))

(defun bqm-fetch-tables-tabulated-list (dataset-name)
  (let ((tables (bigquery-fetch-tables dataset-name)))
    (mapcar
     (lambda (e) (let ((table (cdr (assoc 'id e))))
                   (list nil (vector (cons table (bqm-tab-list-table-button-props table))
                                     (cdr (assoc 'type e))))))
     tables)))

(defun bigquery-fetch-schema (table-name)
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string (format "bq show --schema %s" table-name)))))

(defun bqm-fetch-tabulated-schema-list (table-name)
  (let ((schema (bigquery-fetch-schema table-name)))
    (mapcar (lambda (e) (list nil (vector (cdr (assoc 'name e))
                                          (cdr (assoc 'type e)))))
            schema)))

(defun bigquery-show-datasets ()
  (interactive)
  (let ((buf (get-buffer-create bigquery-datasets-buffer-name))
        (dataset-list (bqm-fetch-tabulated-datasets-list)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("id" . nil))
      (setq tabulated-list-format [("id" 50 nil) ("location" 5 nil)])
      (setq tabulated-list-entries dataset-list)
      (tabulated-list-mode))
    (let ((height (min 10 (+ (length dataset-list) 2))))
      (display-buffer-below-selected buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bigquery-show-tables (dataset-name)
  (interactive)
  (let ((buf (get-buffer-create bigquery-tables-buffer-name))
        (table-list (bqm-fetch-tables-tabulated-list dataset-name)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("id" . nil))
      (setq tabulated-list-format [("id" 100 nil) ("type" 5 nil)])
      (setq tabulated-list-entries table-list)
      (tabulated-list-mode))
    (let ((height (min 10 (+ (length table-list) 2))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bigquery-show-schema (table-name)
  (interactive)
  (let ((buf (get-buffer-create bigquery-schema-buffer-name))
        (schema-list (bqm-fetch-tabulated-schema-list table-name)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("name" . nil))
      (setq tabulated-list-format [("name" 20 nil) ("type" 5 nil)])
      (setq tabulated-list-entries schema-list)
      (tabulated-list-mode))
    (let ((height (min 10 (+ (length schema-list) 2))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

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
         '("`.+`" . 'font-lock-constant-face)
         '("--.*$" . 'font-lock-comment-face)
         (bigquery-font-lock-keyword-builder 'font-lock-keyword-face bigquery-keywords)
         (bigquery-font-lock-keyword-builder 'font-lock-function-name-face bigquery-function-names))))

(defvar bigquery-font-lock-keywords
  (eval-when-compile bigquery-font-lock-keywords))

(defvar bigquery-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for bigquery-mode")

(defvar bigquery-project-id
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "gcloud config get-value project")))

(defun bigquery-set-current-project-id ()
  (setq mode-name (format "BigQuery[%s]" bigquery-project-id)))

(defvar bigquery-sql-indentation-offsets-alist
  `((case-clause +)
    ,@sqlind-default-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-indentation-offsets-alist
                  bigquery-sql-indentation-offsets-alist)))

(defun bigquery-mode ()
  "Major mode for editing bigquery scripts"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bigquery-mode-syntax-table)
  (use-local-map bigquery-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bigquery-font-lock-keywords))
  (setq major-mode 'bigquery-mode)
  (bigquery-set-current-project-id)
  (run-hooks 'bigquery-mode-hook)
  (sqlind-minor-mode)
  )

(provide 'bigquery-mode)

;;; bigquery-mode.el ends here
