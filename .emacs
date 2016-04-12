;;;;;;;;;;;;;;;;;;;;;;;; modular .emacs file ;;;;;;;;;;;;;;;;;;;;;;;;;
;; core = ~/.emacs.d/.emacs <this file>
;; with a symlink as ~/.emacs
;;
;; elpa (and builtin) config module hierachy:
;; elpa modules = ~/.emacs.d/elpa-init-modules-enabled/<link>
;; actual elpa modules files are in ~/.emacs.d/elpa-init-modules-available/<file>
;; and elpa symlinks reside in ~/.emacs.d/elpa-init-modules-enabled/
;;
;; personal el config module hierarchy:
;; el modules = ~/.emacs.d/el-init-modules-enabled/<link>
;; actual el modules files are in ~/.emacs.d/el-init-modules-available/<file>
;; and el symlinks reside in ~/.emacs.d/el-init-modules-enabled/
;;
;; adding a link to the real module enables the module
;;

;; REMINDER: emacs -batch -f batch-byte-compile *.el

;;;;;;;;;;;; disable standard default.el (for debugging) ;;;;;;;;;;;;;
;(setq inhibit-default-init t)

;; file-types and their associated modes
;; for font locking and formatting
(setq auto-mode-alist
      (append '(
                (".*[mM]akefile.*" . makefile-mode)
                ("\.bash_profile" . sh-mode)
                ("\.bashrc" . sh-mode)
                ("\.cshrc" . sh-mode)
                ("\.emacs"  . emacs-lisp-mode)
                ("\.tcshrc" . sh-mode)
                ("\.xemacs" . emacs-lisp-mode)
                ("\\.HTML\\'" . sgml-mode)
                ("\\.HTM\\'" . sgml-mode)
                ("\\.[CH]\\'" . c++-mode)
                ("\\.[Gg][Ii][Ff]\\'" . image-mode)
                ("\\.[Jj][Pp][Ee]?[Gg]\\'" . image-mode)
                ("\\.[Pp][Nn][Gg]\\'" . image-mode)
                ("\\.[Tt][Ii][Ff][Ff]?\\'" . image-mode)
                ("\\.[fF]90\\'" . f90-mode)
                ("\\.[sS]\\'" . asm-mode)
                ("\\.[tT]e[xX]\\'" . tex-mode)
                ("\\.ad[abs]\\'" . ada-mode)
                ("\\.asm\\'" . asm-mode)
                ("\\.awk\\'" . awk-mode)
                ("\\.bas\\'" . basic-mode)
                ("\\.bib\\'" . bibtex-mode)
                ("\\.c\\'" . c-mode)
                ("\\.cc\\'" . c++-mode)
                ("\\.cl\\'" . lisp-mode)
                ("\\.clm\\'" . lisp-mode)
                ("\\.cm\\'" . lisp-mode)
                ("\\.cpp\\'" . c++-mode)
                ("\\.el\\'" . emacs-lisp-mode)
                ("\\.exp\\'" . tcl-mode)
                ("\\.f\\(?:or\\)?\\'" . fortran-mode)
                ("\\.f\\(or\\)?\\'" . fortran-mode)
                ("\\.h\\'" . c-mode)
                ("\\.hh\\'" . c++-mode)
                ("\\.hpp\\'" . c++-mode)
                ("\\.htm\\'" . sgml-mode)
                ("\\.html\\'" . sgml-mode)
                ("\\.hx\\'" . c++-mode)
                ("\\.java\\'" . java-mode)
                ("\\.lex\\'" . c-mode)
                ("\\.lisp\\'" . lisp-mode)
                ("\\.ltx\\'" . latex-mode)
                ("\\.lua\\'" . lua-mode)
                ("\\.m\\'" . matlab-mode)
                ("\\.p[lm]\\'" . perl-mode)
                ("\\.p\\(?:as\\)?\\'" . pascal-mode)
                ("\\.p\\(as\\)?\\'" . pascal-mode)
                ("\\.php\\'" . php-mode)
                ("\\.ps\\'" . postscript-mode)
                ("\\.py\\'" . python-mode)
                ("\\.rules\\'" . conf-unix-mode)
                ("\\.scm\\'" . scheme-mode)
                ("\\.sh\\'" . sh-mode)
                ("\\.sql\\'" . sql-mode)
                ("\\.text\\'" . text-mode)
                ("\\.txt\\'" . text-mode)
                ("\\.vbs\\'" . basic-mode)
                ("\\.y\\'" . c-mode)
                ("\\.yacc\\'" . c-mode)
                ("\\.yy\\'" . c-mode)
                ) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;; fonts and font-locking ;;;;;;;;;;;;;;;;;;;;;;;
;;(set-default-font "-adobe-courier-medium-r-normal--*-140-*-*-m-*-iso8859-1")
(require 'font-lock)
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(setq font-lock-mode-maximum-decoration t)
(setq font-lock-maximum-decoration
      '((sh-mode . 3) (python-mode . 3)))

;; faces coloration
(load "facemenu")

;;====================================================================
;;;;;;;;;;;;;;;;; I N T E R F A C E   H O T K E Y S ;;;;;;;;;;;;;;;;;;
;;====================================================================

;; set delete key functions
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; editor related hotkeys
(global-set-key [(meta g)]  `goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;; sort selected lines
(global-set-key [(control c) (control l)] 'sort-lines)

;;========================================================
;;;;;;;;;;;;;; Defaults for various things ;;;;;;;;;;;;;;;
;;========================================================

;; no splash screen
(setq inhibit-startup-message t)

;; start in text mode
(setq initial-major-mode
      (lambda ()
        (text-mode)
        (turn-on-auto-fill)
        (font-lock-mode t)))

;; autosave: normal
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/normal")))
(add-hook 'before-save-hook 'time-stamp)
(setq auto-save-default t)
(setq auto-save-interval 100)
(setq auto-save-timeout 300)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 3)
(setq kept-old-versions 2)
(setq version-control t)

;; colum number display
(setq column-number-mode t)

;; single line/column scrolling
(setq scroll-step 1)
(setq scroll-right 1)
(setq scroll-left 1)

;; turn off the bell
(setq visible-bell t)

;; do not wrap code lines
(set-default 'truncate-lines t)

;; allow showing truncated lines
;(require 'auto-show)
;(auto-show-mode 1)
;(setq-default auto-show-mode 1)

;; replace yes and no with y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; text selection mode
(setq transient-mark-mode 't)

;; don't automatically add newline
(setq next-line-add-newlines nil)

;; remove all trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; run emacs as a server
(server-start)

;; remove annoying client kill prompt
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; final window size
(setq default-frame-alist '((width . 235) (height . 69)))

;; setup local lisp load dirs
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/el/"))
  (normal-top-level-add-subdirs-to-load-path))

;; load all enabled init modules
(dolist (file (directory-files "~/.emacs.d/elpa-init-modules-enabled" t ".+\\.el$"))
  (load-file file))

;; load all enabled personal (or non-packaged) elisp files
(dolist (file (directory-files "~/.emacs.d/el-init-modules-enabled" t ".+\\.el$"))
  (load-file file))


;;=============================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom set vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;=============================================================================
(setq custom-file "~/.emacs.d/.custom.el")
(load-file custom-file)
