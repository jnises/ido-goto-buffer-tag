;; to use do something like:
;; (add-to-list 'load-path "~/.emacs.d/el/ido-goto-buffer-tag/")
;; (when (require 'ido-goto-buffer-tag nil t) (global-set-key (kbd "C-c i") 'ido-goto-buffer-tag))

(require 'cl)

(defun overlay-to-marker (overlay)
  "convert an overlay to a marker, or just pass through if not an overlay"
  (if (overlayp overlay)
      (let ((marker (make-marker)))
        (set-marker marker (overlay-start overlay) (overlay-buffer overlay)))
    ;; else
    overlay))

(defun ido-goto-buffer-tag-filter (tag)
  "filters tags
return t if the tag should be included"
  (not (eq (semantic-tag-class tag) 'include)))

(defun ido-goto-buffer-tag-get-name (tag)
  "Get the name that identifies tag."
  (concat (semantic-tag-name tag) (when (semantic-tag-get-attribute tag :prototype-flag) "*proto")))

(defun ido-goto-buffer-tag-get-unique-tag (tags)
  "Return a version of tags where all the names are unique."
  (reduce (lambda (result entry)
            (let* ((name (car entry))
                   (uniquename name)
                   (number 0))
              (while (assoc uniquename result)
                (setq number (+ 1 number))
                (setq uniquename (concat name "<" (number-to-string number) ">")))
              (append result (list (cons uniquename (cdr entry))))))
          tags
          :initial-value ()))

(defun ido-goto-buffer-tag-assoc-from-semantic (tags)
  "Convert a list of semantic tags to a list of key value pairs"
  (mapcan ; mapcar that concatenates
   (lambda (tag)
     (let* ((tagname (ido-goto-buffer-tag-get-name tag))
            (result 
             (list (cons tagname 
                         (overlay-to-marker (semantic-tag-overlay tag)))))
            (members 
             (semantic-tag-type-members tag)))
       (when members
         (let ((subtags (mapcar (lambda (symbol)
                                  (cons 
                                   (concat 
                                    tagname 
                                    "::" 
                                    (car symbol)) 
                                   (cdr symbol)))
                                (ido-goto-buffer-tag-assoc-from-semantic members))))
           (when (> (length subtags) 0)
             (setq result (append result subtags)))))
       result))
   (remove-if-not 'ido-goto-buffer-tag-filter tags)))

(defun ido-goto-buffer-tag ()
  "use ido completion to select which tag in this buffer to jump to"
  (interactive)
  (let ((tags (ido-goto-buffer-tag-get-unique-tag 
               (ido-goto-buffer-tag-assoc-from-semantic (semantic-fetch-tags)))))
    (goto-char (cdr (assoc (ido-completing-read "Symbol? " (mapcar (lambda (pair)
                                                                     (car pair))
                                                                   tags)) tags)))))

(provide 'ido-goto-buffer-tag)
