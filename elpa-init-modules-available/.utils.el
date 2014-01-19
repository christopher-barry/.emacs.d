;;; .utils.el --- utility function loaded by .emacs

;; Copyright (C) 2014  Christopher Barry

;; Author: Christopher Barry <cbarry@infinux.org>
;; Keywords:

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

;; set of generic utility functions loaded as .emacs module

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; auto-insert filename into header _emacs field
(global-set-key [(control f8)] 'update-filename-in-header)

;; convert DOS file to UNIX
(global-set-key [(control x) (control u)] 'turn-to-unix)

;; jump to matching parenthesis
(global-set-key [%] 'match-paren)

;; switch active buffer
(global-set-key [(meta control tab)] `switch-to-other-buffer)

;; find regexp in multiple buffers
(global-set-key [(control f9)] 'all-occur)

;; find regexp in multiple buffers of specific mode
(global-set-key [(control f10)] 'mode-occur)

;; find regexp in multiple buffers of specific filetype
(global-set-key [(control f11)] 'type-occur)

;; query replace in open buffers
(global-set-key [(control f12)] 'query-replace-in-open-buffers)

;; indent/outdent selected block
(global-set-key [(meta right)] 'shift-right)
(global-set-key [(meta left)] 'shift-left)

;; browse kill ring
(global-set-key [(control c) (k)] 'browse-kill-ring)

;;====================================================================
;;;;;;;;;;;;;;;;;;;; L A M B D A   H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; reload disk file
(global-set-key [f5]
                '(lambda () "Refresh the buffer from the disk (prompt if modified)."
                   (interactive)
                   (revert-buffer t (not (buffer-modified-p)) t)))

;; paste into emacs from X buffer
(global-set-key [(shift insert)]
                '(lambda () "Paste X buffer into emacs"
                   (interactive)
                   (insert (x-get-selection))))

;; copy from emacs into X buffer
(defun x-own-selection (s) (x-set-selection `PRIMARY s))
(global-set-key [(control insert)]
                '(lambda () "Copy emacs region into X buffer"
                   (interactive)
                   (x-own-selection (buffer-substring (point) (mark)))))


;;====================================================================
;;;;;;;;;;;;;;;;; U T I L I T Y   F U N C T I O N S ;;;;;;;;;;;;;;;;;;
;;====================================================================

;; insert current filename in header :: [(control f8)]
;; function replaces the string 'THIS_FILENAME' with the current file name.
(defun update-filename-in-header ()
  (interactive)
  (save-excursion
    (while (search-forward "THIS_FILENAME" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (downcase (file-name-nondirectory buffer-file-name)))
        (subst-char-in-region (point-min) (point-max) ?. ?_)))))


;; strip windows chars from text :: [(control c) (control u)]
(defun turn-to-unix ()
  (interactive)
  (set-buffer-file-coding-system 'iso-8859-1-unix))

;; jump to matching parenthesis :: [%]
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; switch buffers :: [(meta control tab)]
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; find occurances of regexps in all buffers :: [(control f9)]
(require 'cl)
(defun all-occur (rexp)
  "Search all buffers for REXP."
  (interactive "MRegexp: ")
  (multi-occur (buffer-list) rexp))

;; find occurances of regexps in MODE buffers :: [(control f10)]
(defun mode-occur (mode rexp)
  "Search all buffers with major mode MODE for REXP."
  (interactive (list (read-command "Mode: ")
                     (read-string "Regexp: ")))
  (multi-occur (remove-if (lambda (buf)
                            (set-buffer buf)
                            (not (eq major-mode mode)))
                          (buffer-list))
               rexp))

;; find occurances of regexps in EXTENSION buffers :: [(control f11)]
(defun type-occur (extension rexp)
  "EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REXP."
  (interactive "MExtension: \nMRegexp: ")
  (multi-occur-by-filename-regexp (concat ".*\." extension) rexp))

;; Query Replace in open Buffers :: [(control f12)]
(defun query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open buffers"
  (interactive "sQuery Replace in open Buffers: \nsquery with: ")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (beginning-of-buffer)
       (query-replace arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))

;; indent/outdent region :: [(shift <right/left arrow>)]
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))
(defun shift-right ()
  (interactive)
  (shift-region 1))
(defun shift-left ()
  (interactive)
  (shift-region -1))

;; enable browsing the kill-ring :: [(control c) (k)]
(require 'browse-kill-ring)


;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;; N O   H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; load the current environment
(defun env-line-to-cons (env-line)
  "convert a string of the form \"VAR=VAL\" to a
cons cell containing (\"VAR\" . \"VAL\")."
  (if (string-match "\\([^=]+\\)=\\(.*\\)" env-line)
      (cons (match-string 1 env-line) (match-string 2 env-line))))
(defun interactive-env-alist (&optional shell-cmd env-cmd)
  "launch /usr/bin/env or the equivalent from an interactive
shell, parsing and returning the environment as an alist."
  (let ((cmd (concat (or shell-cmd "/bin/bash -ic")
                     " "
                     (or env-cmd "/usr/bin/env"))))
    (mapcar 'env-line-to-cons
            (remove-if
             (lambda (str)
               (string-equal str ""))
             (split-string (shell-command-to-string cmd) "[\r\n]")))))
(defun setenv-from-cons (var-val)
  "set an environment variable from a cons cell containing
two strings, where the car is the variable name and cdr is
the value, e.g. (\"VAR\" . \"VAL\")"
  (setenv (car var-val) (cdr var-val)))
(defun setenv-from-shell-environment (&optional shell-cmd env-cmd)
  "apply the environment reported by `/usr/bin/env' (or env-cmd)
as launched by `/bin/bash -ic' (or shell-cmd) to the current
environment."
  (mapc 'setenv-from-cons (interactive-env-alist shell-cmd env-cmd)))


;; allow template insertions (on file extension load)
(require 'autoinsert)
(auto-insert-mode)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/src/templates/") ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion
(define-auto-insert "\.sh" "sh-template")
(define-auto-insert "\.bash" "bash-template")

;; make speedbar do horizontal scrolling
(defadvice speedbar-frame-mode (after fix-hscroll activate)
  (set (make-local-variable 'auto-hscroll-mode) t))
(load "speedbar")

;; setup additional package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; embedded python interpreter
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; dired extensions
(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))


;;; .utils.el ends here
