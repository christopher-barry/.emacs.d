;;; .sh-mode.el --- .emacs init module for shell code

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

;;; Commentary: Loads elements for shell editing

;;

;;; Code:

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;; H O T K E Y S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;;====================================================================
;;;;;;;;;;;;;;;;;;;;;;;;; L O A D   F I L E ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;====================================================================

;; turn on sh-mode for scripts without .sh extension
(setq executable-prefix "#!"
      sh-shell-file     "/bin/bash"
      sh-indent-comment t)

;; font-locking of operators
(font-lock-add-keywords
 'sh-mode
 `((,(concat "[ \t]+" (regexp-opt
     '("-d" "-e" "-f" "-l" "-n" "-p" "-x" "-z" "-a" "-o" "-i" "-t" "-T" "-S" "!" "!=" "==" "=~" "-eq" "-ne" "-lt" "-gt" "-le" "-ge" ) 'paren) "[ \t]+")
   . font-lock-constant-face)))

;; font-locking of operators - unspaced
(font-lock-add-keywords
 'sh-mode
 `((,(regexp-opt
     '( "&" "<" ">" "|" ";" "+=" ) 'paren)
   1 font-lock-constant-face prepend)))

;; font-locking of operators - unspaced
(font-lock-add-keywords
 'sh-mode
 `((,(regexp-opt
     '( "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "@" "*") 'paren)
   1 font-lock-variable-name-face prepend)))

;; font-locking of bash var delimiters - unspaced
(font-lock-add-keywords
 'sh-mode
 `((,(regexp-opt
     '( "$" "{" "}" ) 'paren)
   1 font-lock-preprocessor-face prepend)))

;; font-locking of bash brackets and parens - unspaced
(font-lock-add-keywords
 'sh-mode
 `((,(regexp-opt
     '( "[" "]" "(" ")" "set\ \[-+\]x" "typeset" ) 'paren)
   1 font-lock-warning-face prepend)))

;; font-locking of bash type designators
(font-lock-add-keywords
 'sh-mode
 `((,(concat "[ \t]*" (regexp-opt
     '( "declare" "local" "source" "readonly" ) 'paren) "[ \t]+")
   . font-lock-type-face)))

;; font-locking of shell commands
(font-lock-add-keywords
 'sh-mode
 `((,(concat "[ \t]*" (regexp-opt
     '( "sed" "awk" "tee" "basename" "ln" "mkdir" "ssh-keygen" "puttygen"
        "find" "ls" "cat" "egrep" "grep" "mv" "cp" "chmod" "chown" "tar"
        "zip" "gzip" "bzip2" "unzip" "gunzip" "bunzip2" "rm" "mount" "umount") 'paren) "[ \t]+")
   . font-lock-builtin-face)))

;; project specific fontlocking
;; sftp-manager
(font-lock-add-keywords
 'sh-mode
 `((,(concat "[ \t]+" (regexp-opt
     '("ifError" "ifLevel" "out" "sendEmail" "log" "debug" "getMetrics"
       "rndup" "isLessThan" "DotPathName" "SlashPathName" "trim" "underSpace"
       "Usage" "makeTmpFile" "validateOptions" "initArrayMask" "showArrayMask"
       "processLongOptions" "parseInputField" "sanitize" "toLower") 'paren) "[ \t]+")
   . font-lock-type-face)))
;; uniramfs
(font-lock-add-keywords
 'sh-mode
 `((,(concat "[ \t]+" (regexp-opt
     '( "out" "log" "err" "warn" "trim" "clean" "in_debug_mode" "is_defined" "print_stack" ) 'paren) "[ \t]+")
   . font-lock-type-face)))

;; handle escape sequences in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; replace all tabs with spaces on save
(add-hook 'sh-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook
                       (lambda ()
                         (untabify (point-min) (point-max))))))

;;; .sh-mode.el ends here
