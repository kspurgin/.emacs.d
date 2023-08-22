;; Functions to define human-readable size column for ibuffer.
;; This is required by the ibuffer and ibuffer-vc setup in init.
;; From: https://www.emacswiki.org/emacs/IbufferMode#h5o-11

(defun ajv/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))
   )
  )

(defun ajv/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes)))
  )

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size"
	 :inline t
	 :summarizer
	 (lambda (column-strings)
	   (let ((total 0))
	     (dolist (string column-strings)
	       (setq total
		     ;; like, ewww ...
		     (+ (float (ajv/human-readable-file-sizes-to-bytes string))
			total)))
	     (ajv/bytes-to-human-readable-file-sizes total)))	 ;; :summarizer nil
	 )
  (ajv/bytes-to-human-readable-file-sizes (buffer-size)))
