;;; .tramp.el --- .emacs config module for tramp

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

;;; Commentary: set up tramp as .emacs config module

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================


;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; autosave: tramp
(setq tramp-auto-save-directory "~/.emacs.d/backups/tramp")

;; load tramp
(require 'tramp)
(setq shell-prompt-pattern "^H")
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer t)

;; allow hopping through proxies
;(add-to-list 'tramp-default-proxies-alist
;             '("root@192.168.0.242" nil "/ssh:root@@monitor:")
;             '("root@192.168.0.70" nil "/ssh:root@@monitor:")
;             )

;;; .tramp.el ends here
