(defun personal-laptop ()
  (equal (system-name) "spore"))
(defun work-laptop ()
  (equal (system-name) "Kristina-MBP"))

(setq user-full-name "Kristina M. Spurgin")
(when (personal-laptop)
  (setq user-mail-address "kristina@le-champignon.net")
  (message "You are on your personal laptop.")
)
(when (work-laptop)
  (setq user-mail-address "kristina.spurgin@lyrasis.org")
    (message "You are on your work laptop.")
)

(when (work-laptop)
  (set-keyboard-coding-system nil)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'super)
)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)
(message "use-package is set up now")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-refresh-contents))
(message "Loaded package sources")

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(when (work-laptop)
       (load "LYRASIS_macros")
       (message "work-related macros loaded"))

(cond ((display-graphic-p)
       (use-package darktooth-theme
	 :ensure t)
       (load-theme 'darktooth t)
       (message "loaded theme")
       ))

(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-restore-frames t) ;;doesn't seem to work, at least on Ubuntu.
(setq desktop-buffers-not-to-save
     (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	        "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	        "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(setq ring-bell-function 'ignore)

(use-package cperl-mode
  :mode "\\.p[lm]\\'"
  :interpreter "perl"
  :config (load "cperl-setup"))

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :custom-face
  (enh-ruby-string-delimiter-face ((t (:foreground "wheat1"))))
  )

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode))
  )

(use-package org
  :ensure t
  )

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-initial-inputs-alist nil)
)

(use-package counsel
    :ensure t
    :bind (("M-x" . counsel-M-x))
)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

(use-package visual-regexp-steroids
  :ensure t
  :ensure visual-regexp
  :bind (("C-c r" . vr/replace)
	 ("C-c q" . vr/query-replace)
	 ("C-M-R" . vr/isearch-backward)
	 ("C-M-S" . vr/isearch-forward))
  )

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1)
  )

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode))
  )

(when (work-laptop)
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2019/bin/x86_64-darwin"))
	(add-to-list'exec-path "/usr/local/texlive/2019/bin/x86_64-darwin"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package auto-org-md
  :ensure t
)

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; other misc appearance settings
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; do not show the toolbar (big icons across top)
(tool-bar-mode 0)

;; highlight matching parentheses
(setq blink-matching-paren nil)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

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
; everytime bookmark is changed, automatically save it
; from http://ergoemacs.org/emacs/bookmark.html
(setq bookmark-save-flag 1)

;; Sentences end with ONE space
;; from http://pages.sachachua.com/.emacs.d/Sacha.html
(setq sentence-end-double-space nil)

;; Just insert one tab when I hit tab.
;; From http://www.pement.org/emacs_tabs.htm
(global-set-key (kbd "TAB") 'self-insert-command)

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

; Move line or region up or down with M-up/down arrow
(use-package move-text
  :ensure t
  :config
   (move-text-default-bindings))

;; I don't need backup files. I never have used them, and they just cause clutter
(setq make-backup-files nil)

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

;; Allows traversing the mark ring without hitting C-u C-SPC all the time.
;; Found at http://endlessparentheses.com/faster-pop-to-mark-command.html
(setq set-mark-command-repeat-pop t)

;; Make file and buffer name completion case insensitive
;; From http://endlessparentheses.com/improving-emacs-file-name-completion.html
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; tramp
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'tramp)
(cond ((string-equal system-type 'gnu/linux)
       (setq tramp-default-method "ssh"))
      ((string-equal system-type 'darwin)
       (setq tramp-default-method "ssh"))
      ((string-equal system-name 'windows-nt)
       (setq tramp-default-method "plink")))

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

;; human readable file sizes
;; from http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
(setq dired-listing-switches "-Alh")

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
(cond ((string-equal system-type 'darwin)
(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    '("~/org/meetings.org"
		      "~/org/notes.org"))))
;; from http://orgmode.org/manual/Tracking-TODO-state-changes.html
(setq org-todo-keywords
      '((sequence "TODO(t!)" "INPROGRESS(p!)" "DELEGATED(a@/!)" "WAITING(w@/!)" "|" "DONE(d!)" )
	(sequence "|" "CANCELED(c@)" )))
))

(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 0)
(setq org-use-property-inheritance (quote ("COLLECTION" "VENDOR")))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-log-into-drawer t)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Change tasks to INPROGRESS when clocking in
(setq org-clock-in-switch-to-state "INPROGRESS")
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

(setq org-clock-idle-time 5)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-log-note-clock-out nil)
(setq org-duration-format 'h:mm)

(setq org-startup-indented nil)
(setq org-hide-leading-stars nil)

(setq org-refile-targets (quote (("cspace.org" :maxlevel . 3)
                                 ("migrations.org" :level . 3)
                                 ("projects.org" :level . 3))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t))
          t)

;; prevents accidentally editing hidden text when the point is inside a folded region
(setq org-catch-invisible-edits 'error)

(setq org-cycle-include-plain-lists t)

; insert blank lines before headings but not new list items
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . auto)))

; The following setting creates a unique task ID for the heading in the PROPERTY drawer when I use C-c l. This allows me to move the task around arbitrarily in my org files and the link to it still works.
; From http://doc.norang.ca/org-mode.html

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["#3C3836" "#FB4934" "#84BB26" "#FABD2F" "#83A598" "#D3869B" "#3FD7E5" "#EBDBB2"])
;;  '(custom-safe-themes
;;    (quote
;;     ("c1709b576b0bdf885e380f8f787c2063ea3fb55be6c92400d4361014430b4efa" "272e45b301d3a8ffaad475191f9a406361e70b1fb60acb42354184cf290e04f5" default)))
;;  '(package-selected-packages
;;    (quote
;;     (visual-regexp-steroids yasnippet markdown-mode flymd yaml-mode auto-org-md use-package php-mode org move-text darktooth-theme auto-compile)))
;;  '(pos-tip-background-color "#36473A")
;;  '(pos-tip-foreground-color "#FFFFC8"))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight bold :height 98 :size 13 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/org/work.org" "~/org/cspace.org" "~/org/migrations.org" "~/org/meetings.org")))
 '(package-selected-packages
   (quote
    (yaml-mode nxml-mode enh-ruby-mode yafolding adoc-mode php-mode yasnippet visual-regexp-steroids use-package move-text markdown-mode darktooth-theme auto-org-md auto-compile))))
