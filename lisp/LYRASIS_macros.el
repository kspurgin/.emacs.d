(fset 'reformat-date-fields
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217765 47 115 105 110 103 108 101 32 return 47 115 105 110 103 108 101 32 62 32 return 33 134217788 134217765 62 32 108 97 116 101 115 116 return 62 33554464 108 97 116 101 115 116 32 62 33554464 return 33 134217788 134217765 32 32 return 32 return 33 134217788 134217765 81 85 65 76 73 backspace backspace backspace backspace backspace 113 117 97 108 105 102 105 101 114 32 118 97 108 117 101 return 118 97 108 117 101 return 33 134217788 134217765 113 117 97 108 105 102 105 101 114 32 117 110 105 116 return 117 110 105 116 return 33 134217788] 0 "%d")) arg)))


(fset 'upcase-fieldname
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([167772179 32 62 32 91 97 45 122 93 return 67108896 2 24 21] 0 "%d")) arg)))

;;; Expected data:
;;;   - name of optionlist on first line OF FILE (use scratch)
;;;   - list of values immediately below, one per line
;;;   - no blank space between optionlist name and value list
;;;   - plain value list - not wrapped in quotes and commas
;;;   - add line break after last value in list!
;;; Start with cursor at beginning of first line of file
(defalias 'optlist-messages
   (kmacro "C-n C-SPC C-e M-w M-> \" C-y \" : SPC { <return> SPC SPC i d : SPC \" o p t i o n . M-< C-SPC C-e M-w M-> C-y . M-< C-n C-SPC C-e M-w M-> C-y \" , <return> d e f a u l t M e s s a g e : SPC \" C-y \" , <return> <backspace> <backspace> } , <return> M-< C-n C-k C-k M-<"))

;;; Start with list of values, one per line.
;;; Cursor should be at beginning of first line in list
(defalias 'optlist-values
   (kmacro "\" C-e \" , C-n C-a"))

(defalias 'ke-outline-registry
   (kmacro "C-s o C-g C-x ( M-s o \\ . n a m e s p a c e \\ | SPC + r e g i s t e r <return> C-x o"))
