# Major Mode for Editing and Executing BigQuery Scripts

`bigquery-mode` is a GNU Emacs major mode for editing and executing
Google BigQuery scripts with the following features:

- Syntax Highlighting
- Indentation based on
  [`sql-indent.el`](https://github.com/alex-hhh/emacs-sql-indent/blob/master/sql-indent.org)
- Listing of available projects, datasets, and tables with their
  schemas.
- (Dry) running queries.
  
## Installation

Before using BigQuery Mode, make sure that you have installed the
[Google Cloud SDK](https://cloud.google.com/sdk) and that both
`gcloud` and `bq` are on you `PATH`.

To install BigQuery mode, simply copy `bigquery-mode.el` and
`bqm-names.el` to your Load Path. Usually, this is `~/.emacs.d/lisp/`.
  
## Usage

| Shortcut  | Action                            |
|-----------|-----------------------------------|
| `C-c s`   | List and set Google Cloud Project |
| `C-c t`   | List datasets                     |
| `C-RET`   | Run query                         |
| `C-M-RET` | Dry run query                     |
