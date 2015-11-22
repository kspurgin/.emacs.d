;; added 2015-04-20
;; add .emacs.d/lisp and its subdirectories to load-path
;; from http://emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; added 2015-05-20
;; source: http://ergoemacs.org/emacs/emacs_package_system.html
;; adds MELPA repository to list of package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

; added 20130326
; source: http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(load-theme 'tango t)
(message "loaded theme")

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

;; make emacs automatically notice any changes made to files on disk
;; especially useful for making reftex notice changes to bibtex files
;; http://josephhall.org/nqb2/index.php/2009/04/11/reftex-1
;; Fri May 22 19:32:12 EDT 2009
(global-auto-revert-mode t)

;;; auto-create non-existing directories to save files
;;; http://atomized.org/2008/12/emacs-create-directory-before-saving/
;;; Sun Dec 14 00:04:46 EST 2008

(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Desktop saving
;; load, at startup, the buffers you were editing when you last quit Emacs.
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'file-name-history)

;; ;; Do not save/reopen certain kinds of buffers

(setq desktop-buffers-not-to-save
     (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	        "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
	        "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

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
;; open drag/dropped files in new buffer
;; added 20130628, http://www.emacswiki.org/emacs/DragAndDrop
(define-key global-map [ns-drag-file] 'ns-find-file)


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

;; scratch should be in text mode
;; 2014-03-13 - http://emacsworld.blogspot.com/2008/06/changing-default-mode-of-scratch-buffer.html
(setq initial-major-mode 'text-mode)

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; dired stuff
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; do not open a bajillion buffers to navigate file system
(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
   (function
    (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
   ;; we're good to go; just add our bindings
   (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; MISCELLANEOUS TOOLS
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;;############################################################################
;; org-mode
;;############################################################################
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; added 2015-05-20 from custom.el and http://pages.sachachua.com/.emacs.d/Sacha.html
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/org/esm.org"
		      "~/org/tasks_and_projects.org"))))

; added 20150519 18:04 from http://orgmode.org/manual/Tracking-TODO-state-changes.html
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "INPROGRESS(i!)" "|" "DONE(d!)" "CANCELED(c@!)" "DELEGATED(o@!)")))

(custom-set-variables
 ;; only these properties should be inherited
 ;; added 2015-05-28 from http://orgmode.org/manual/Property-syntax.html
 '(org-use-property-inheritance (quote ("COLLECTION" "VENDOR")))
 ;; limit what is shown in the agenda
 ;; added 2015-06-29 from http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/
 '(org-deadline-warning-days 0)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
)

; never insert blank lines for me
; from http://stackoverflow.com/questions/28351465/emacs-orgmode-do-not-insert-line-between-headers
; 2015-11-19
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;############################################################################
;#   Emacs config (Recommended) from Appendix C of "Perl Best Practices"    #
;#     Copyright (c) O'Reilly & Associates, 2005. All Rights Reserved.      #
;#  See: http://www.oreilly.com/pub/a/oreilly/ask_tim/2001/codepolicy.html  #
;############################################################################
; Added 20130612

;; Use cperl mode instead of the default perl mode
(defalias 'perl-mode 'cperl-mode)
 
;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)
 
;; Use 4 space indents via cperl mode
(custom-set-variables
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t))
 
;; Insert spaces instead of tabs
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))
 
;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)
 
;; Use % to match various kinds of brackets...
;; See: http://www.lifl.fr/~hodique/uploads/Perso/patches.el
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))
 
;; Load an applicationtemplate in a new unattached buffer...
(defun application-template-pm ()
  "Inserts the standard Perl application template"  ; For help and info.
  (interactive "*")                                 ; Make this user accessible.
  (switch-to-buffer "application-template-pm")
  (insert-file "~/.code_templates/perl_application.pl"))
;; Set to a specific key combination...
;;(global-set-key "\C-ca" 'application-template-pm)
 
;; Load a module template in a new unattached buffer...
(defun module-template-pm ()
  "Inserts the standard Perl module template"       ; For help and info.
  (interactive "*")                                 ; Make this user accessible.
  (switch-to-buffer "module-template-pm")
  (insert-file "~/.code_templates/perl_module.pl"))
;; Set to a specific key combination...
(global-set-key "\C-cm" 'module-template-pm)
 
;; Expand the following abbreviations while typing in text files...
(abbrev-mode 1)
 
(define-abbrev-table 'global-abbrev-table '(
    ("pdbg"   "use Data::Dumper qw( Dumper );\nwarn Dumper[];"   nil 1)
    ("phbp"   "#! /usr/bin/perl -w"                              nil 1)
    ("pbmk"   "use Benchmark qw( cmpthese );\ncmpthese -10, {};" nil 1)
    ("pusc"   "use Smart::Comments;\n\n### "                     nil 1)
    ("putm"   "use Test::More 'no_plan';"                        nil 1)
    ))
 
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; copy full path of buffer
;; Added 20150916 from:
;;  http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
(global-set-key "\C-cz" 'show-file-name)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; delete duplicate/repeated buffer lines 
;; sort lines before using since lines have to be one after the other
;; 20091206 01:16 commented out because not working right
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  (defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))
  
  (defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-region-lines (point-min) (point-max)))

  (defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))
  
  (defun uniquify-buffer-lines ()
    "Remove duplicate adjacent lines in the current buffer."
    (interactive)
    (uniquify-region-lines (point-min) (point-max)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; do an incremental search on a regexp and hide lines that match the regexp.
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(require 'hide-lines)
(require 'hidesearch)
;; (global-set-key (kbd "C-c C-s") 'hidesearch)
;; (global-set-key (kbd "C-c C-a") 'show-all-invisible)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; my macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; normalize millennium output - breaks lines at semicolon - will not handle quotes surrounding
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(fset 'normalize-millennium-output
   [?\C-s ?\; ?\C-m ?\C-e ?\C-r ?\; ?\C-m delete ?\C-  ?\C-e ?\C-w return ?\C-y ?\C-p ?\C-a ?\C-  ?\M-f ?\M-w ?\C-n ?\C-a ?\C-y tab ?\C-p ?\C-a])

;; Insert OCLC dagger subfield delimiter!
;; Mix of info from http://www.emacswiki.org/emacs/KeyBindingDiscussion
;;  and http://www.emacswiki.org/emacs/PrefixKey
  (defun insert-oclc-dagger ()
    "Insert OCLC dagger subfield delimiter!"
    (interactive)
    (insert "ǂ"))

(global-set-key (kbd "C-c d") 'insert-oclc-dagger)


;; Replace spaceless dollars with spaced daggers.
(fset 'marcedit-to-oclc-syntax
   [?\C-a ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?\\ ?$ ?\\ ?\( ?. ?\\ ?\) return ?  ?\C-c ?d ?\\ ?1 ?  return ?\C-a ?\C-  ?\C-e ?\M-w return])

(global-set-key (kbd "C-c C-o") 'marcedit-to-oclc-syntax)

(fset 'normalize-titles
   [?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?\\ ?\( ?\[ ?. ?, ?\; ?: ?\' ?\" ?\( ?\) ?\\ ?/ ?\] ?\\ ?| ?- ?\\ ?| ?\[ ?\\ ?| ?\] ?\\ ?\) return ?  return ?\M-< ?\M-% ?& return ?a ?n ?d return ?! ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?  ?+ return return ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?  ?+ ?$ return return ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?  ?+ return ?  return ?\M-< ?\M-< ?\C-  ?\M-> ?\C-x ?\C-l ?\M-<])

(fset 'iii-global-update-fields-to-tab-delimited
   [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?^ ?\C-q tab ?\[ ?0 ?- ?9 ?\] ?+ ?\C-q tab ?. ?\C-q tab ?  ?\C-q tab ?G ?r ?o ?u ?p ?: ?  ?. ?  ?. ?. ?. ?. ?. ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\\ ?\( ?\C-e ?\\ ?\) ?\\ ?\( ?. ?* ?\\ ?\) ?E ?x ?p ?a ?n ?d ?e ?d ?: ?  ?f ?a ?l ?s ?e ?  ?E ?n ?t ?r ?i ?e ?s ?: ?  ?\[ ?0 ?- ?9 ?\] ?+ return ?\\ ?1 ?\C-q tab ?\\ ?2 return])

(fset 'marcedit-copy-from-find-cleanup
   [?\M-< ?\C-  ?\M-> ?\C-a ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-x ?r ?k ?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?  ?* ?\C-q tab ?J ?u ?m ?p ?  ?t ?o ?  ?R ?e ?c ?. ?* ?$ return return]
)
