;;; bigquery-mode.el --- A BigQuery major mode

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

;; Major mode for writing and executing Google BigQuery Queries.

;;; Code:

(require 'json)
(require 'sql-indent)

(defconst bqm-projects-buffer-name "* BigQuery Projects *")
(defconst bqm-dry-run-buffer-name "* BigQuery Dry Run *")
(defconst bqm-datasets-buffer-name "* BigQuery Datasets *")
(defconst bqm-tables-buffer-name "* BigQuery Tables *")
(defconst bqm-schema-buffer-name "* BigQuery Schema *")
(defconst bqm-table-buffer-name "* BigQuery Table *")
(defconst bqm-query-buffer-name "* BigQuery Query *")

(defun bqm-fetch-projects ()
  (let ((projects-json (json-read-from-string (shell-command-to-string "gcloud projects list --format=json"))))
    (mapcar (lambda (e) (list nil (vector (cdr (assoc 'projectId e))
                                          (cdr (assoc 'name e))
                                          (cdr (assoc 'projectNumber e)))))
            projects-json)))

(defun bqm-tab-list-project-button-props (project-id)
  (list 'action (lambda (b)
                  (bqm-set-project (button-get b 'project-id))
                  (quit-window))
        'project-id project-id))

(defun bqm-set-project (project-id)
  (shell-command (format "gcloud config set project %s" project-id))
  (bqm-set-current-project-id))

(defun bqm-fetch-tabulated-project-list ()
  (let ((projects (json-read-from-string (shell-command-to-string "gcloud projects list --format=json"))))
    (mapcar
     (lambda (e) (let ((projectId (cdr (assoc 'projectId e)))
                       (name (cdr (assoc 'name e)))
                       (projectNumber (cdr (assoc 'projectNumber e))))
                   (list nil
                         (vector (cons projectId (bqm-tab-list-project-button-props projectId))
                                 name
                                 projectNumber))))
     projects)))

(defun bqm-list-projects ()
  (interactive)
  (let ((buf (get-buffer-create bqm-projects-buffer-name))
        (project-list (bqm-fetch-tabulated-project-list)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("projectId" . nil))
      (setq tabulated-list-format [("projectId" 30 t) ("name" 30 t) ("projectNumber" 10 t)])
      (setq tabulated-list-entries project-list)
      (tabulated-list-mode))
    (let ((height (min 40 (max 10 (+ (length project-list) 2)))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bqm-project-id ()
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "gcloud config get-value project")))

(defun bqm-set-current-project-id ()
  (let ((bufs (buffer-list)))
    (dolist (b bufs)
      (with-current-buffer b
        (if (string-match "bigquery-mode" (symbol-name major-mode))
            (setq mode-name (format "BigQuery[%s]" (bqm-project-id))))))))

(defun bqm-run-query ()
  (interactive)
  (bqm-set-current-project-id)
  (let ((query (buffer-substring-no-properties (point-min) (point-max))))
    (bqm-execute-query query)))

(defun bqm-fetch-datasets ()
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string "bq ls --format=json"))))

(defun bqm-tab-list-dataset-button-props (dataset-name)
  (list 'action (lambda (b) (bqm-show-tables (button-get b 'dataset))) 'dataset dataset-name))

(defun bqm-fetch-tabulated-datasets-list ()
  (let ((datasets (bqm-fetch-datasets)))
    (mapcar
     (lambda (e) (let ((dataset (cdr (assoc 'id e))))
                   (list nil (vector (cons dataset (bqm-tab-list-dataset-button-props dataset))
                                     (cdr (assoc 'location e))))))
     datasets)))
    
(defun bqm-fetch-tables (dataset-name)
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string (format "bq ls --format=json %s" dataset-name)))))

(defun bqm-tab-list-table-button-props (table-name)
  (list 'action (lambda (b) (bqm-show-schema (button-get b 'table))) 'table table-name))

(defun bqm-fetch-tables-tabulated-list (dataset-name)
  (let ((tables (bqm-fetch-tables dataset-name)))
    (mapcar
     (lambda (e) (let ((table (cdr (assoc 'id e))))
                   (list nil (vector (cons table (bqm-tab-list-table-button-props table))
                                     (cdr (assoc 'type e))))))
     tables)))

(defun bqm-fetch-schema (table-name)
  (let ((json-object-type 'alist))
    (json-read-from-string (shell-command-to-string (format "bq show --schema %s" table-name)))))

(defun bqm-fetch-tabulated-schema-list (table-name)
  (let ((schema (bqm-fetch-schema table-name)))
    (mapcar (lambda (e) (list nil (vector (cdr (assoc 'name e))
                                          (cdr (assoc 'type e)))))
            schema)))

(defun bqm-show-datasets ()
  (interactive)
  (let ((buf (get-buffer-create bqm-datasets-buffer-name))
        (dataset-list (bqm-fetch-tabulated-datasets-list)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("id" . nil))
      (setq tabulated-list-format [("id" 50 nil) ("location" 5 nil)])
      (setq tabulated-list-entries dataset-list)
      (tabulated-list-mode))
    (let ((height (min 40 (max 10 (+ (length dataset-list) 2)))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bqm-show-tables (dataset-name)
  (interactive)
  (let ((buf (get-buffer-create bqm-tables-buffer-name))
        (table-list (bqm-fetch-tables-tabulated-list dataset-name)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("id" . nil))
      (setq tabulated-list-format [("id" 100 nil) ("type" 5 nil)])
      (setq tabulated-list-entries table-list)
      (tabulated-list-mode))
    (let ((height (min 40 (max 10 (+ (length table-list) 2)))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bqm-show-schema (table-name)
  (interactive)
  (let ((buf (get-buffer-create bqm-schema-buffer-name))
        (schema-list (bqm-fetch-tabulated-schema-list table-name)))
    (with-current-buffer buf
      (setq tabulated-list-sort-key '("name" . nil))
      (setq tabulated-list-format [("name" 20 nil) ("type" 5 nil)])
      (setq tabulated-list-entries schema-list)
      (tabulated-list-mode))
    (let ((height (min 40 (max 10 (+ (length schema-list) 2)))))
      (display-buffer-at-bottom buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(defun bqm-fetch-text-from-buffer (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun bqm-dry-run-query ()
  (interactive)
  (bqm-set-current-project-id)
  (let ((query (buffer-substring-no-properties (point-min) (point-max))))
    (bqm-dry-run query)))
                     
(defun bqm-dry-run (query)
  (let ((buf (get-buffer-create bqm-dry-run-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (let ((process (start-process "query" buf "bq" "query" "--dry_run" "--quiet" "--nouse_legacy_sql" "--format=json")))
        (set-process-sentinel process
                              (lambda (p m)
                                (if (string-prefix-p "finished" m t)
                                    (message (bqm-format-dry-run-result (bqm-fetch-text-from-buffer (process-buffer p))))
                                  (message (replace-regexp-in-string "\n" " " (bqm-fetch-text-from-buffer (process-buffer p)))))
                                ))
        (process-send-string process query)
        (process-send-eof process)
        (process-send-eof process)))))

(defun bqm-format-dry-run-result (json-res)
  (let* ((m (json-read-from-string json-res))
         (statistics (cdr (assoc 'statistics m)))
         (bytes (cdr (assoc 'totalBytesProcessed statistics))))
    (format "Processed bytes: %s" (bqm-format-bytes (string-to-number bytes)))))

(defun bqm-execute-query (query)
  "Runs query, returns corresponding job id"
  (let ((job-id (bqm-generate-new-job-id)))
    (bqm-submit-query query job-id)
    job-id))

(defun bqm-format-bytes (bytes)
  (let* ((units ["B" "KB" "MB" "GB" "TB" "PB"])
         (counter 0)
         (small bytes))
    (while (and (<= counter (length units))
                (< 1024 small))
      (setq counter (+ counter 1))
      (setq small (/ bytes (expt 1024 counter))))
    (format "%s%s" small (aref units counter))))

(defun bqm-generate-new-job-id ()
    (format-time-string "%s"))

(defun bqm-submit-query (query job-id)
  (let ((buf (get-buffer-create bqm-query-buffer-name)))
    (display-buffer-below-selected buf '((window-height . fit-window-to-buffer)))
    (with-current-buffer buf
      (fundamental-mode)
      (erase-buffer)
      (view-mode)
      (select-window (get-buffer-window buf))
      (let ((process (start-process "query" buf "bq" "query" "--quiet" "--nouse_legacy_sql" "--job_id" job-id query)))
        (set-process-sentinel process
                              (lambda (p m)
                                (goto-char (point-min))
                                (fit-window-to-buffer)))))))

(defun bqm-table-list-format (table-name)
  (let ((schema (bqm-fetch-schema table-name)))
    (apply 'vector
           (mapcar (lambda (f) (list (cdr (assoc 'name f)) 20 nil))
                   schema))))

(defun bqm-table-head (table-name)
  (json-read-from-string (shell-command-to-string (format "bq head --format=json %s" table-name))))

(defvar bqm-table-list-format-header)

(defun bqm-tab-list-table (table-name)
  (let ((bqm-table-list-format-header (bqm-table-list-format table-name))
        (content (bqm-table-head table-name))
        (row-to-entry (lambda (row) (list nil (apply 'vector (mapcar (lambda (f) (cdr (assoc (intern (car f)) row)))
                                bqm-table-list-format-header))))))
    (mapcar row-to-entry content)))

(defun bqm-show-table (table-name)
  (interactive)
  (let ((buf (get-buffer-create bqm-table-buffer-name))
        (content (bqm-table-head table-name)))
    (with-current-buffer buf
      (setq tabulated-list-format (bqm-table-list-format table-name))
      (setq tabulated-list-entries (bqm-tab-list-table table-name))
      (tabulated-list-mode))
    (let ((height (min 40 (max 10 (+ (length content) 2)))))
      (display-buffer-below-selected buf '((window-height . fit-window-to-buffer))))
    (let ((w (get-buffer-window buf)))
      (select-window w))))

(add-to-list 'auto-mode-alist '("\\.sql\\'" . bigquery-mode))

(require 'bqm-names)

(eval-when-compile
  (defvar bqm-font-lock-keywords))

(eval-and-compile
  (defun bqm-font-lock-keyword-builder (face keywords)
    (cons (concat "\\<" (regexp-opt keywords) "\\>") face)))

(defconst bqm-block-comment-regexp "/\\*\\(.\\|\n\\)*?\\*/")

(eval-when-compile
  (setq bqm-font-lock-keywords
        (list
         '("--.*$" . 'font-lock-comment-face)
         ; '('bqm-block-comment-regexp . 'font-lock-comment-face)
         '("`.+`" . 'font-lock-constant-face)
         (bqm-font-lock-keyword-builder 'font-lock-keyword-face bqm-keywords)
         (bqm-font-lock-keyword-builder 'font-lock-function-name-face bqm-function-names))))

(defvar bqm-font-lock-keywords
  (eval-when-compile bqm-font-lock-keywords))

(defvar bqm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for bigquery-mode")

(defvar bqm-sql-indentation-offsets-alist
  `((case-clause +)
    ,@sqlind-default-indentation-offsets-alist))

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-indentation-offsets-alist
                  bqm-sql-indentation-offsets-alist)))

(defvar bigquery-mode-hook nil)

(defvar bqm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") 'bqm-list-projects)
    (define-key map (kbd "<C-M-return>") 'bqm-dry-run-query)
    (define-key map (kbd "<C-return>") 'bqm-run-query)
    (define-key map (kbd "C-c t") 'bqm-show-datasets)
    map)
  "Keymap for BigQuery major mode")

(defun bigquery-mode ()
  "Major mode for editing bigquery scripts"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table bqm-mode-syntax-table)
  (use-local-map bqm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(bqm-font-lock-keywords))
  (setq major-mode 'bigquery-mode)
  (bqm-set-current-project-id)
  (run-hooks 'bigquery-mode-hook)
  (sqlind-minor-mode)
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip "--[:blank:]*")
  )

;; (add-hook 'bigquery-mode
;;           (lambda ()
;;             (set (make-local-variable 'comment-start) "--")
;;             (set (make-local-variable 'comment-end) "")))

(provide 'bigquery-mode)

;;; bigquery-mode.el ends here
