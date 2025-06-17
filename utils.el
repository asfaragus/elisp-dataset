;; -------------------------------------
;; Org dataset to Json dataset converter
;; -------------------------------------
;;
;; This function parses the dataset org files and converts them to a
;; json format as separated buffer.
;;
;; Usage example:
;; M-x org-to-json


(require 'json)
(require 'org-element)

(defun org-to-json ()
  "Convert the current Org buffer to JSON format and display in a new buffer."
  (interactive)
  (let ((json-entries '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
	(let ((title (org-element-property :raw-value headline))
	      (instruction "")
	      (input "")
	      (output ""))
	  (when (eq (org-element-property :level headline) 1)
	    (let ((section (org-element-contents headline)))
	      (dolist (element section)
		(when (eq (car element) 'headline)
		  (let ((subhead-title (org-element-property :raw-value element))
			(subhead-contents (org-element-interpret-data (org-element-contents element))))
		    (cond
		     ((string= subhead-title "instruction")
		      (setq instruction subhead-contents))
		     ((string= subhead-title "input")
		      (setq input subhead-contents))
		     ((string= subhead-title "output")
		      (setq output subhead-contents)))))))
	    (push `((instruction . ,instruction) (output . ,output) (input . ,input)) json-entries)))))
    (with-current-buffer (get-buffer-create "*Org to JSON*")
      (erase-buffer)
      (insert (json-encode (reverse json-entries)))
      (json-pretty-print-buffer)
      (display-buffer (current-buffer)))))
