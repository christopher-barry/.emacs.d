;;; .planner.el --- .emacs config module for planner

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

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

(global-set-key [(meta shift f9)]   'planner-create-task-from-buffer)
(global-set-key [(meta shift f10)]  'planner-delete-task)
(global-set-key [(meta shift f11)]  'planner-copy-or-move-task)
(global-set-key [(meta shift f12)]  'planner-update-task)
(global-set-key [(meta up)]         'planner-raise-task)
(global-set-key [(meta shift up)]   'planner-raise-task-priority)
(global-set-key [(meta down)]       'planner-lower-task)
(global-set-key [(meta shift down)] 'planner-lower-task-priority)

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; projects
(setq planner-project "tetralogic")

(setq muse-project-alist
      '(("tetralogic"
         ("~/Plans"
          :default "TaskPool"
          :major-mode planner-mode
          :visit-link planner-visit-link)
         (:base "planner-xhtml"
                :path "~/public_html/Plans"))))

(require 'planner)
(require 'remember)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

;; start it
(plan)

;;; .planner.el ends here
