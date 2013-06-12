; added 20130326
; sets .emacs.d as load-path in a recursive manner. all subdirectories are also added
; keeps emacs from thinking it has been started from .emacs.d
; http://www.emacswiki.org/emacs-en/LoadPath
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir "~/.emacs.d/")
            (default-directory my-lisp-dir))
         (setq load-path (cons my-lisp-dir load-path))
         (normal-top-level-add-subdirs-to-load-path)))
(message "set .emacs.d as load path")

; added 20130326
; define and load my custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(message "define and load my custom file")

; added 20130326
; source: http://marmalade-repo.org/
; adds open source Marmalade repository to list of package archives
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(message "added custom package archives")

; added 20130326
; source: http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(load-theme 'tango t)
(message "loaded theme")

; added 20130326
; make emacs and cygwin play together
(load-library "cygwinize")
(message "cygwinized")

; added 20130327
; Auto-saved backup files
; Save in a directory, not all over the place.
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; other misc appearance settings
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; do not show the toolbar (big icons across top)
(tool-bar-mode 0)

;; highlight matching parentheses
(show-paren-mode t)

;; automatically turn on sytax highlighting
(global-font-lock-mode 1)

;; show column numbers 20100625 12:20
(column-number-mode)

;; do not blink the cursor
(blink-cursor-mode 0)

;; stretch the cursor to show the size of the character under cursor
;; useful for seeing tabs and other weird whitespace
(setq x-stretch-cursor t)

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; MISCELLANEOUS BEHAVIOR SETTINGS
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;; do not disable things for me.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; will allow you to type just "y" instead of "yes" when you exit.
(fset 'yes-or-no-p 'y-or-n-p)

;; will disallow creation of new lines when you press the "arrow-down-key" at end of the buffer.
(setq next-line-add-newlines nil)

;; will make the display of date and time persistent.
(setq display-time-day-and-date t) (display-time)

;; don't show that stupid message on the scratch file
(setq initial-scratch-message nil)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; MISCELLANEOUS TOOLS
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; move lines/regions up and down
;; source: http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;; added: 20110318 17:43
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
