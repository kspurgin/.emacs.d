

;############################################################################
;#   Emacs config (Recommended) from Appendix C of "Perl Best Practices"    #
;#     Copyright (c) O'Reilly & Associates, 2005. All Rights Reserved.      #
;#  See: http://www.oreilly.com/pub/a/oreilly/ask_tim/2001/codepolicy.html  #
;############################################################################
; Added 20130612

;; turn autoindenting on
(global-set-key "\r" 'newline-and-indent)

;; Insert spaces instead of tabs
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
	     (setq cperl-tab-always-indent t)
	     (setq cperl-indent-parens-as-block t)
	     (setq cperl-indent-level 4)
	     (setq cperl-continued-statement-offset 4)
	     (setq cperl-close-paren-offset -4)))

;; Set line width to 78 columns...
(setq fill-column 78)
(setq auto-fill-mode t)

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
