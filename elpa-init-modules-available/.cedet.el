;;; .cedet.el --- .emacs init module for code browser and friends

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

;;; Commentary: Loads elements of the Emacs IDE

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; CEDET
(require 'cedet)
(semantic-load-enable-excessive-code-helpers)

;; ECB :: The Emacs Code Browser
(require 'ecb)

;;; .cedet.el ends here
