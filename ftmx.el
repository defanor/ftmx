;;; ftmx.el --- Command search and execution

;; Copyright (C) 2016 defanor

;; Author: defanor <defanor@uberspace.net>
;; Maintainer: defanor <defanor@uberspace.net>
;; Created: 2016-08-16
;; Keywords: help, matching
;; Homepage: https://github.com/defanor/ftmx
;; Package-Requires: ((emacsql "2.0") (emacsql-sqlite "1.0") (seq "2.16"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This is inspired by the atom text editor's "command palette"
;; feature: a quick interactive search through available commands by
;; their descriptions.

;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite)
(require 'seq)

(defgroup ftmx nil
  "M-x with full text search"
  :prefix "ftmx-"
  :group 'ftmx)

(defcustom ftmx-db-sqlite-path "/tmp/obarray.db"
  "A path to the sqlite database used to store commands."
  :group 'ftmx
  :type 'file)

(defcustom ftmx-db-chunk-size 500
  "An amount of records to instert with a single DB query."
  :group 'ftmx
  :type 'integer)

(defvar ftmx-db nil
  "An emacsql database.")

(defvar ftmx-initialized nil
  "Indicates the state of `ftmx', used to initialize on the first
  `ftmx' if `ftmx-init' wasn't called.")

(defvar ftmx-offset 0
  "An offset used for rotation of search results.")

(defvar ftmx-current-input nil
  "Minibuffer input as of the last completion.")

(defvar ftmx-current-command nil
  "Command name that was selected on `ftmx-current-input'.")

;;;###autoload
(defun ftmx-init ()
  "Initialize the database: drop the table if it exists, create a
new one one, fill it with commands found in `obarray'."
  (interactive)
  (let ((init-msg "Initializing the ftmx database..."))
    (message init-msg)
    (setq ftmx-db (emacsql-sqlite ftmx-db-sqlite-path))
    (emacsql ftmx-db [:drop-table-if-exists commands])
    (emacsql ftmx-db [:create-virtual-table commands :using fts4
                                            ([name description])])
    (let ((records '()))
      (mapatoms (lambda (atom)
                  (when (commandp atom)
                    (push (let ((sn (symbol-name atom)))
                            (vector sn (concat sn "\n" (documentation atom))))
                          records))))
      (mapc (lambda (chunk)
              (emacsql ftmx-db [:insert :into commands
                                        :values $v1]
                       chunk))
            (seq-partition records ftmx-db-chunk-size)))
    (emacsql-close ftmx-db)
    (message (concat init-msg " Done."))))

(defun ftmx-find-matches (where what)
  "Find in a field WHERE matches of WHAT."
  (setq ftmx-db (emacsql-sqlite ftmx-db-sqlite-path))
  (let ((ret (mapcar 'car
          (emacsql ftmx-db [:select [name]
                                    :from commands
                                    :where (match $i1 $s2)]
                   where
                   what))))
    (emacsql-close ftmx-db)
    ret))

(defun ftmx-find (str)
  "Find maching strings in names, or in descriptions if nothing
is found there."
  (let ((query (make-symbol str)))
    (or (ftmx-find-matches 'name query)
        (ftmx-find-matches 'description query))))

(defun ftmx-annotate (str)
  "Annotate a command. Currently just showing the first line of
its description."
  (let ((sym (intern str)))
    (concat (if (string-equal str ftmx-current-command) "*" "")
            ": "
            (if (functionp sym)
                (let ((doc (documentation sym)))
                  (if doc
                      (let ((lines (split-string doc "\n")))
                        (car lines))
                    "no documentation"))
              "not a function"))))

(defun ftmx-read (str pred flag)
  "A function to be used with `completing-read'."
  (setq ftmx-current-input str)
  (cond
   ((null flag)
    (let* ((cr (ftmx-find str))
           (cr-len (length cr))
           (offset (if (= cr-len 0)
                       0
                     (mod ftmx-offset cr-len)))
           (commands (append (seq-drop cr offset)
                             (seq-take cr offset)))
           (cname (car commands)))
      (setq ftmx-current-command cname)
      (ftmx-exhibit
       (cond ((null commands) "not found")
             ((= (length commands) 1)
              (concat "*" cname (ftmx-annotate cname)))
             (t (concat cname (ftmx-annotate cname)))))
      (concat (string-trim str) " ")))
     ((eq flag t) (ftmx-find str))
     ((consp flag) '(boundaries 0 . 0))
     ((equal flag 'metadata)
      `(metadata . ((annotation-function . ftmx-annotate)
                    (display-sort-function . (lambda (x) x)))))
     (t nil)))

;;;###autoload
(defun ftmx ()
  "M-x-style command execution with full text search"
  (interactive)
  (setq ftmx-offset 0)
  (when (null ftmx-initialized)
    (ftmx-init)
    (setq ftmx-initialized 't))
  (add-hook 'minibuffer-setup-hook #'ftmx-minibuffer-setup)
  (let ((str (completing-read "ftmx: " 'ftmx-read)))
    (remove-hook 'minibuffer-setup-hook #'ftmx-minibuffer-setup)
    (when (not (string-equal str ftmx-current-input))
      (ftmx-read str nil nil)))
  (execute-extended-command nil ftmx-current-command))

(defvar ftmx-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the list of completions.")

(defun ftmx-exhibit (text)
  "Show `ftmx' completions."
  (save-excursion
    (goto-char (point-max))
    (move-overlay ftmx-overlay (point) (point) (current-buffer))
    (let ((str (concat " {" text "}")))
      (put-text-property 0 1 'cursor t str)
      (overlay-put ftmx-overlay 'after-string str))))

(defun ftmx-result-set (func)
  "Change the offset of a result by applying a function FUNC to
the current offset."
  (setq ftmx-offset (funcall func ftmx-offset))
  (ftmx-read ftmx-current-input nil nil))

(defun ftmx-next-result ()
  "Select the next result."
  (interactive)
  (ftmx-result-set '1+))

(defun ftmx-previous-result ()
  "Select the previous result."
  (interactive)
  (ftmx-result-set '1-))

(defvar ftmx-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-n] 'ftmx-next-result)
    (define-key map [?\C-p] 'ftmx-previous-result)
    map)
  "Keymap used by `ftmx' in the minibuffer.")

(defun ftmx-minibuffer-setup ()
  "Setup `ftmx' keybindings in the minibuffer, should be set on
`minibuffer-setup-hook'."
  (use-local-map (make-composed-keymap ftmx-minibuffer-map
                                       (current-local-map))))

(provide 'ftmx)
;;; ftmx.el ends here
