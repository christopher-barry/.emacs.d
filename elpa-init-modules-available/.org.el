;;; .org.el --- .emacs config module for org-mode

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

;;; Commentary: sets up org-mode as .emacs init module

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

(global-set-key [(control c) (R)] 'org-capture)

(define-key global-map [(control c) (r)]
	(lambda () (interactive) (org-capture nil "r")))

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; setup templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline  "~/Documents/windows/earborg/org/todo.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("i" "Ideas" entry (file+headline "~/Documents/windows/earborg/org/ideas.org" "Ideas")
         "* %?")
        ("n" "Names" entry (file+headline "~/Documents/windows/earborg/org/names.org" "Names")
         "* %?")))

;; allow completion of tags
(add-hook 'org-capture-mode-hook
               (lambda ()
                 (set (make-local-variable
                       'org-complete-tags-always-offer-all-agenda-tags)
                      t)))

(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map [(control c) (x)] 'org-todo-state-map)

     (define-key org-todo-state-map "s"
       '(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "f"
       '(lambda nil (interactive) (org-todo "FEEDBACK")))
     (define-key org-todo-state-map "v"
       '(lambda nil (interactive) (org-todo "VERIFY")))
     (define-key org-todo-state-map "w"
       '(lambda nil (interactive) (org-todo "WAITING")))
     (define-key org-todo-state-map "l"
       '(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "h"
       '(lambda nil (interactive) (org-todo "HOLD")))
     (define-key org-todo-state-map "d"
       '(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "r"
       '(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "c"
       '(lambda nil (interactive) (org-todo "CANCELLED")))))


;; todo file locations
'(org-agenda-files (quote ("~/Documents/windows/earborg/org/todo.org")))
'(org-default-notes-file "~/Documents/windows/earborg/org/notes/.notes")
'(org-directory "~/Documents/windows/earborg/org")

;; hide heading elipsis
'(org-ellipsis nil)

;; hide prefix stars
'(org-startup-indented t)

;; hide markers
'(org-hide-emphasis-markers t)

;; allow single key commands
'(org-fast-tag-selection-single-key (quote expert))

;; increase level indentation
'(org-odd-levels-only t)

;; footnote format
'(org-footnote-auto-adjust t)

;; what/when to log
'(org-log-done (quote time))
'(org-log-redeadline nil)
 
;; load org modules
'(org-modules (quote (org-docview org-habit org-info org-irc org-mhe org-rmail org-w3m)))

;; states a task can be in
'(org-todo-keywords (quote ((sequence "TODO(t!)" "STARTED(s!)" "FEEDBACK(f!/!)" "VERIFY(v!/!)" "WAITING(w@/!)" "DELEGATED(l@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "DEFERRED(r@)" "CANCELED(c@)"))))

;; agenda specific settings
'(org-agenda-ndays 7)
'(org-deadline-warning-days 14)
'(org-agenda-show-all-dates t)
'(org-agenda-skip-deadline-if-done t)
'(org-agenda-skip-scheduled-if-done t)
'(org-agenda-start-on-weekday nil)
'(org-reverse-note-order t)
'(org-agenda-custom-commands
   (quote (("s" todo "STARTED" nil)
           ("f" todo "FEEDBACK" nil)
           ("v" todo "VERIFY" nil)
           ("w" todo "WAITING" nil)
           ("l" todo "DELEGATED" nil)
           ("h" todo "HOLD" nil)
           ("d" todo "DONE" nil)
           ("r" todo "DEFERRED" nil)
           ("c" todo "CANCELLED" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))


;;; .org.el ends here
