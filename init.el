(setq user-full-name "Kristina M. Spurgin")

(defun init-computer-os ()
  (cond ((equal (system-name) "secunit") 'nix)
	((equal (system-name) "spore") 'nix)
	((equal (system-name) "Kristina-Macbook-Pro.local") 'mac)
	((equal (system-name) "Kristina-MBP") 'mac)))

(defun init-computer-context ()
  (cond ((equal (init-computer-os) 'nix) 'personal)
	((equal (init-computer-os) 'mac) 'work)))

(when (equal (init-computer-context) 'personal)
  (setq user-mail-address "kristina@le-champignon.net")
  (message "You are on your personal laptop."))
(when (equal (init-computer-context) 'work)
  (setq user-mail-address "kristina.spurgin@lyrasis.org")
  (message "You are on your work laptop."))

(when (equal (init-computer-os) 'mac)
  (set-keyboard-coding-system nil)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'super))

(when (version< emacs-version "29.1")
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (setq use-package-verbose t)
  (require 'use-package)
  (setq load-prefer-newer t)
  (message "use-package is set up for Emacs <29.1"))

(when (>= emacs-major-version 29)
  (message "use-package is included by default")
  (setq package-install-upgrade-built-in t))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (message "exec-path-from-shell initialized"))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(package-refresh-contents)
(message "Loaded package sources")

(when (>= emacs-major-version 29)
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(add-to-list 'load-path "~/.emacs.d/lisp")

(when (equal (init-computer-context) 'work)
  (load "LYRASIS_macros")
  (message "work-related macros loaded"))

(setq x-stretch-cursor t)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

(tool-bar-mode 0)
(setq initial-scratch-message nil)

(column-number-mode)
(setq display-time-day-and-date t) (display-time)

(cond ((display-graphic-p)
       (use-package darktooth-theme
	 :config
	 (load-theme 'darktooth t)
	 (darktooth-modeline)
	 )
       (message "loaded theme")
       ))

(global-font-lock-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "TAB") 'self-insert-command)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq standard-indent 2)

(setq next-line-add-newlines nil)

(use-package move-text
  :config
  (move-text-default-bindings))

(setq initial-major-mode 'text-mode)

(setq set-mark-command-repeat-pop t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'before-save-hook
	  #'(lambda ()
	      (or (file-exists-p (file-name-directory buffer-file-name))
		  (make-directory (file-name-directory buffer-file-name) t))))

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

(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '((".*" . "~/.saves"))
      ;; auto-save-file-name-transforms `((".*" "~/.saves" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "coverage")))

(global-set-key (kbd "M-g 8")
		(lambda () (interactive) (move-to-column 80)))

;; Make file and buffer name completion case insensitive
;; From http://endlessparentheses.com/improving-emacs-file-name-completion.html
(setq read-file-name-completion-ignore-case nil)
(setq read-buffer-completion-ignore-case nil)

(add-hook 'prog-mode-hook 'show-paren-mode)
(setq blink-matching-paren nil)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package column-enforce-mode)

;; (setq major-mode-remap-alist
;;       '((enh-ruby-mode . ruby-ts-mode)
;; 	(ruby-mode . ruby-ts-mode)
;; 	(js2-mode . js-ts-mode)
;; 	(python-mode . python-ts-mode)))

(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|z?log\\(in\\|out\\)\\)\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.\\(shrc\\|zshrc\\|m?kshrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-ts-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(add-hook 'js-mode-hook (lambda () (electric-indent-local-mode -1)))

(use-package cperl-mode
  :mode "\\.p[lm]\\'"
  :interpreter "perl"
  :config (load "cperl-setup"))

(add-hook 'ruby-ts-mode-hook 'column-enforce-mode)
(setq ruby-after-operator-indent nil)
(setq ruby-aligned-chain-calls nil)
(setq ruby-align-to-stmt-keywords nil)
(setq ruby-block-indent nil)
(setq ruby-method-call-indent nil)

(use-package ruby-refactor
  :config
  (add-hook 'ruby-ts-mode-hook 'ruby-refactor-mode-launch))

(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-method-params-indent 0)

(use-package rubocop
  :config
  (setq rubocop-autocorrect-on-save t)
  (add-hook 'ruby-mode-hook #'rubocop-mode)
  (add-hook 'ruby-ts-mode-hook #'rubocop-mode)
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode))

(use-package nhexl-mode)

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

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq org-hide-emphasis-markers t)
(setq org-fontify-emphasized-text t)

(setq org-hide-leading-stars nil)

(setq org-startup-indented nil)

(setf org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(custom-set-faces
 '(org-headline-done ((t (:foreground "gray50")))))
(setq org-fontify-done-headline t)

(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

(setq org-fold-catch-invisible-edits "smart")

(setq org-special-ctrl-a/e t)

(with-eval-after-load 'org
  (defun kms/expand-elisp-src-block () (interactive)
	 (insert "#+begin_src emacs-lisp\n")
	 (insert "  \n")
	 (insert "#+end_src\n")
	 (previous-line 2)
	 (end-of-line))

  (define-key org-mode-map (kbd "C-& C-s") `kms/expand-elisp-src-block))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(when (equal (init-computer-context) 'work)
  (setq org-agenda-files
	(delq nil
	      (mapcar (lambda (x) (and (file-exists-p x) x))
		      '("~/org/cspace.org"
			"~/org/islandora.org"
			"~/org/meetings.org"
			"~/org/migrations.org"
			"~/org/notes.org"
			"~/org/work.org"
			"~/org/mig/wpl_westerville_public_library_cs.org")))))

(setq org-agenda-show-all-dates t)

(setq org-agenda-skip-deadline-if-done t)

(setq org-agenda-skip-scheduled-if-done t)

(setq org-clock-idle-time 5)

(setq org-clock-out-remove-zero-time-clocks t)

(setq org-log-note-clock-out nil)

(setq org-duration-format 'h:mm)

(setq org-deadline-warning-days 0)

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(setq org-refile-use-outline-path 'file)

(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords
      '((sequence "TODO(t!)" "INPROGRESS(p!)" "DELEGATED(a@/!)" "WAITING(w@/!)" "|" "DONE(d!)" )
	(sequence "|" "CANCELED(c@)" )
	(sequence "ASK(s!)" "|" "ANSWERED(n@/!)" )
	(sequence "MTG(m)" "|" )
	(sequence "ONGOING(o)" "|" )))

(setq org-enforce-todo-dependencies t)

(setq org-enforce-todo-checkbox-dependencies t)

(setq bookmark-save-flag 1)

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)
	 ("\\.asciidoc\\'" . adoc-mode))
  :config
  (progn
    (set-face-attribute  'adoc-meta-face
			 nil
			 :foreground "pink1"
			 :height 100)

    (set-face-attribute  adoc-meta-hide-face
			 nil
			 :foreground "gray40"
			 :height 100)
    )
  )

(use-package htmlize
  )

(when (equal (init-computer-context) 'work)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2019/bin/x86_64-darwin"))
  (add-to-list'exec-path "/usr/local/texlive/2019/bin/x86_64-darwin"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package auto-org-md)

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'adoc-mode-hook #'yas-minor-mode)
  (add-hook 'fundamental-mode #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'enh-ruby-mode-hook #'yas-minor-mode)
  (add-hook 'ruby-mode-hook #'yas-minor-mode)
  (add-hook 'ruby-ts-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode))

(setq yas-expand-only-for-last-commands (self-insert-command 1))
(define-key yas-minor-mode-map (kbd "=") yas-maybe-expand)

(use-package ivy
  :diminish
  :config
  (ivy-mode t)
  ;; disable default behavior of starting filters with =^
  (setq ivy-initial-inputs-alist nil)
  ;; select entered text with C-p/C-n
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :bind (("M-x" . counsel-M-x))
  )

(require 'ibuffer)
(load "ibuffer-human-readable")
(keymap-global-set "C-x C-b" 'ibuffer)

(use-package ibuffer-vc)

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      (vc-status 16 16 :left)
	      " "
	      vc-relative-file)))

(with-eval-after-load 'ibuffer
  (defun kms-ibuffer/vc-filter-groups ()
    (message "CALLED: kms-ibuffer/vc-filter-groups")
    (ibuffer-vc-generate-filter-groups-by-vc-root))


  (defun kms-ibuffer/set-saved-filter-groups ()
    (message "CALLED: kms-ibuffer/set-saved-filter-groups")
    (setq ibuffer-saved-filter-groups
	  `(("filters"
	     ("magit" (name .".*magit"))
	     ("mig: wpl"
	      (or (filename . "code/mig/wpl-collectionspace-migration")
		  (filename . "data/wpl_westerville_public_library")
		  (filename . "org/mig/wpl_westerville_public_library_cs.org")))
	     ("mig: az-ccp"
	      (or (filename . "code/mig/az_ccp_cspace_migration")
		  (filename . "data/az_ccp")))
	     ("mig: ksu"
	      (or (filename . "code/mig/ksu_collectionspace_migration")
		  (filename . "data/ksu")))
	     ,@(kms-ibuffer/vc-filter-groups)
	     ("meta" (or
		      (basename . "cspace.org")
		      (basename . "islandora.org")
		      (basename . "meetings.org")
		      (basename . "migrations.org")
		      (basename . "work.org")))
	     ("emacs" (or (name . "\\*Messages\\*")
			  (name . "\\*Compile-Log\\*")
			  (name . "\\*Backtrace\\*")
			  (name . "\\*Occur\\*")
			  (name . "\\*Warnings\\*")
			  (name . "\\*emacs\\*")))
	     ("help" (name . "\\*Help\\*"))))))

  (defun kms-ibuffer/switch-ibuffer-group ()
    (kms-ibuffer/set-saved-filter-groups)
    (ibuffer-switch-to-saved-filter-groups "filters"))

  (add-hook 'ibuffer-mode-hook 'kms-ibuffer/switch-ibuffer-group)
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode))

(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

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

(setq dired-listing-switches "-Alh")

(setq dired-auto-revert-buffer t)

(setq dired-clean-up-buffers-too t)

(setq dired-create-destination-dirs "ask")

(setq dired-create-destination-dirs-on-trailing-dirsep t)

(use-package visual-regexp-steroids
  :ensure visual-regexp
  :bind (("C-c r" . vr/replace)
	 ("C-c q" . vr/query-replace)
	 ("C-M-R" . vr/isearch-backward)
	 ("C-M-S" . vr/isearch-forward))
  )

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "bundle")
     (add-to-list 'grep-find-ignored-directories "spring")
     (add-to-list 'grep-find-ignored-directories "storage")
     (add-to-list 'grep-find-ignored-directories "packs")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-create-missing-test-files "t"))

(use-package swiper
  :bind (("C-s" . swiper)))

(fset 'noblame
      (kmacro-lambda-form [?\S-\C-\M-s ?  return backspace ?\C-  ?\C-e ?\C-w ?\C-a ?# ?  ?\C-y return ?\C-e return ?\C-n] 0 "%d"))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq git-commit-style-convention-checks nil))

(setq sentence-end-double-space nil)

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

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; MISCELLANEOUS TOOLS
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


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
 '(package-selected-packages
   (quote
    (yaml-mode nxml-mode enh-ruby-mode yafolding adoc-mode php-mode yasnippet visual-regexp-steroids use-package move-text markdown-mode darktooth-theme auto-org-md auto-compile))))
