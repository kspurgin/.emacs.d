(fset 'reformat-date-fields
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217765 47 115 105 110 103 108 101 32 return 47 115 105 110 103 108 101 32 62 32 return 33 134217788 134217765 62 32 108 97 116 101 115 116 return 62 33554464 108 97 116 101 115 116 32 62 33554464 return 33 134217788 134217765 32 32 return 32 return 33 134217788 134217765 81 85 65 76 73 backspace backspace backspace backspace backspace 113 117 97 108 105 102 105 101 114 32 118 97 108 117 101 return 118 97 108 117 101 return 33 134217788 134217765 113 117 97 108 105 102 105 101 114 32 117 110 105 116 return 117 110 105 116 return 33 134217788] 0 "%d")) arg)))


(fset 'upcase-fieldname
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([167772179 32 62 32 91 97 45 122 93 return 67108896 2 24 21] 0 "%d")) arg)))

(defalias 'optlist-messages
   (kmacro "C-f C-SPC C-e C-b C-b M-w C-f : SPC { <return> i d : SPC ' o p t i o n . d e p a r t m e n t s . C-y ' , <return> d e f a u l t M e s s a g e : SPC ' C-y ' , <return> } C-n C-a"))
