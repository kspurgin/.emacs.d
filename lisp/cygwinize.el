;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Make Emacs work with Cygwin
;; Fri Jun 19 21:46:37 EDT 2009
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;; cygwin-mount.el
;; http://www.khngai.com/emacs/cygwin.php
;; Fri Jun 19 21:51:37 EDT 2009

(setenv "PATH" (concat "C:/cygwin/bin;"
                (getenv "PATH")))

(setq exec-path (cons "c:/cygwin/bin/" exec-path))

(require 'cygwin-mount)
(cygwin-mount-activate)

;; use cygwin bash shell
;; http://www.khngai.com/emacs/cygwin.php
;; Fri Jun 19 21:53:29 EDT 2009

(add-hook 'comint-output-filter-functions
    'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
    'comint-watch-for-password-prompt nil t)
(setq explicit-shell-file-name "bash.exe")

;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)
