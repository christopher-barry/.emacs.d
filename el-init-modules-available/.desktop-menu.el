;;; .desktop-menu.el --- .emacs config module for desktop-menu

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

;;; Commentary: sets up desktop-menu

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; desktop-menu
(global-set-key [f7] 'desktop-menu)

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; save projects with desktop-menu
(add-to-list 'load-path "~/.emacs.d/el/desktop-menu")
(require 'desktop-menu)
(load "desktop-menu")

;;; .desktop-menu.el ends here
