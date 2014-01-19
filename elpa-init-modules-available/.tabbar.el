;;; .tabbar.el --- .emacs config module for tabbar

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

;;; Commentary: set up tabbar as a .emacs module

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; file to file
(global-set-key [f12]                     'tabbar-forward)
(global-set-key [(control meta mouse-5)]  'tabbar-forward)
(global-set-key [f11]                     'tabbar-backward)
(global-set-key [(control meta mouse-4)]  'tabbar-backward)

;; file-group to file-group
(global-set-key [(shift f12)]             'tabbar-forward-group)
(global-set-key [(control shift mouse-5)] 'tabbar-forward-group)
(global-set-key [(shift f11)]             'tabbar-backward-group)
(global-set-key [(control shift mouse-4)] 'tabbar-backward-group)

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; load tabbed buffer list
(require 'tabbar)
(tabbar-mode 1)

;;; .tabbar.el ends here
