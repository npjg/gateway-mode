;;; gateway-mode --- access Bible Gateway from Emacs

;;; Commentary:

;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; URL: http://github.com/npjg/gateway-mode
;; Package-Requires:
;; Version: 0.1
;; Created: 2020-01-05
;; By: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; Keywords: Bible

;;; Code:

(defgroup gateway nil
	"Customization group for gateway-mode")

(defvar gateway-version nil
	"The Bible version to use for interacting with Bible Gateway.
To modify with available versions, use `gateway-set-version'.")

(defvar gateway-inhibit-cookies t
	"If non-nil, inhibit cookies in BibleGateway URL requests.")

(defvar gateway-inhibit-crossrefs nil
	"If non-nil, inhibit BibleGateway cross-references.")

(defvar gateway-inhibit-footnotes nil
	"If non-nil, inhibit BibleGateway footnotes.")

(defvar gateway-inhibit-headings nil
	"If non-nil, inhibit BibleGateway headings.")

(defvar gateway-inhibit-versenums nil
	"If non-nil, inhibit BibleGateway verse numbers.")

(defvar gateway-display-mode-hook nil
  "Normal hook run when entering gateway-display-mode.")

(defun gateway--check-libxml ()
	"Assert that libxml2 is compiled into Emacs."
	(unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2")))

(defun gateway--assert-mode ()
	"Raise an error if the proper data structures are not present,
and return t otherwise."
	(if (boundp 'gateway-data) t
		(user-error "Not a BibleGateway buffer")))

(defun gateway--update-message (entity status)
	"Display a message when a visibility ENTITY is set to STATUS."
	(message (format "%s %sabled in current buffer" entity (if status "dis" "en"))))

(defun gateway--version-completing-read ()
	"Get the static version information from BibleGateway, and
perform a `completing-read' which returns an alist of the
version's abbreviation and full name."
	(gateway--check-libxml)
	(with-current-buffer (url-retrieve-synchronously "https://biblegateway.com" t gateway-inhibit-cookies)
		(let* ((dom (libxml-parse-html-region (point) (point-max)))
					 (options (cddar (dom-by-tag dom 'select)))
					 (results nil))
			(dolist (option (reverse options) results)
				(unless (assoc 'class (cadr option))
					(push (cons (caddr option) (cdr (assoc 'value (cadr option)))) results)))
			(assoc (completing-read "Version: " results nil t) results))))

(defun gateway--refresh-entities (nodes inhibit)
	"Refresh the NODES' display property with INHIBIT."
	(dolist (node nodes) (dom-set-attribute node 'style (format "display:%s" (if inhibit "none" "inline")))))

(defun gateway--shr-tag-span (func &rest r)
	"Set the verse property for each span. Only intended to advise
`shr-tag-span'."
	(let* ((dom (car r))
				 (init (point))
				 (classes (split-string (cdr (assoc 'class (cadr dom))))))
		(apply func r)
		(when (string= (car classes) "text")
			(put-text-property init (point) 'start init)
			(put-text-property init (point) 'end (point))
			(put-text-property init (point) 'verse (cadr classes)))))

(defun gateway--shr-tag-sup (func &rest r)
	"Set the verse property for each sup. Only intended to advise
`shr-tag-sup'."
		(let* ((dom (car r))
					 (init (point))
					 (class (cdr (assoc 'class (cadr dom)))))
			(apply func r)
			(put-text-property init (point) 'class class)))

(defun gateway--shr-tag-a (func &rest r)
	"Set the footnote tooltip text to the actual value of the
footnote. Only intended to advise `shr-tag-a'."
	(let* ((dom (car r))
				 (init (point))
				 (id (cdr (assoc 'href (cadr dom)))))
		(apply func r)
		(when (string-match "^#[fc]..-" id)
			(let* ((text (plist-get gateway-data :text))
						 (rendered (with-temp-buffer (shr-descend (dom-by-id text (substring id 1))) (buffer-substring (point-min) (point-max)))))
				(put-text-property init (point) 'help-echo rendered)))))

(defun gateway--resolve-osis (osis)
	"Convert standard book abbrevation OSIS to human-friendly form."
	(gateway--assert-mode)
	(catch 'found
		(mapc (lambda (book) (if (string= (alist-get 'osis book) osis) (throw 'found (alist-get 'display book))))
					(plist-get gateway-data :books))
		nil))

(defun gateway-fetch-version ()
	"Read version information and return a cons cell with version
	information to be used by `gateway-fetch-version'."
	(let ((version (cdr (gateway--version-completing-read))))
		(cons version (gateway-fetch-bcv version))))

(defun gateway-set-version ()
	"Set the global `gateway-version' with the valid options
provided by BibleGateway."
	(interactive)
	(let ((version (gateway-fetch-version)))
		(setq gateway-version version)
		(message (format "Set global BibleGateway version to %s" (car version)))))

(defun gateway-change-version ()
	"Change the version for the current passage only."
	(interactive)
	(gateway--assert-mode)
	(let ((bcv (plist-get gateway-data :bcv))
				(version (cdr (gateway--version-completing-read))))
		(gateway-fetch-passage bcv (cons version (gateway-fetch-bcv version)) (buffer-name))
		(message "Changed local BibleGateway version to %s" version)))

(defun gateway-fetch-votd (&optional version)
	"Display the verse of the day for VERSION."
	(interactive)
	(unless (or version (car gateway-version))
		(gateway-set-version))
	(with-current-buffer
			(url-retrieve-synchronously
			 (format "https://www.biblegateway.com/votd/get/?format=json&version=%s"
							 (or version (car gateway-version))))
		(goto-char url-http-end-of-headers)
		(let* ((json-object-type 'alist)
					 (json-key-type 'symbol)
					 (json-array-type 'list)
					 (votd (cdar (json-read))))
			(when (assoc 'code votd)
				(user-error (format "BibleGateway: (%s) %s" (cdr (assoc 'code votd)) (cdr (assoc 'message votd)))))
			(message (format "%s (%s; %s)" (cdr (assoc 'text votd)) (cdr (assoc 'display_ref votd)) (cdr (assoc 'version_id votd)))))))

(defun gateway-fetch-bcv (version)
	"Fetch the book information for VERSION."
	(let ((result))
		(with-current-buffer (url-retrieve-synchronously (format "https://www.biblegateway.com/passage/bcv/?version=%s" version))
		  (goto-char url-http-end-of-headers)
		  (let* ((json-object-type 'alist)
						 (json-key-type 'symbol)
						 (json-array-type 'list)
						 (raw (json-read)))
				(if (eq (cdar raw) :json-false)
					(user-error (format "BibleGateway: Incorrect version string"))
					(setq result (mapcar (lambda (book) (assq-delete-all 'chapters book)) (cadar raw))))))
		result))

(defun gateway-fetch-passage (passage &optional version update)
	"Load a PASSAGE from BibleGateway. The global version specified
in `gateway-version' is used, unless VERSION is non-nil. When
called interactively, VERSION is a prefix argument that, when
non-nil, causes the version to be read for the current lookup
only. Otherwise, VERSION holds a result from
`gateway-fetch-version' for the current lookup only. UPDATE holds
the name of an existing BibleGateway buffer to be updated, which
will be verified valid before writing."
	(interactive "MReference: \nP")
	(gateway--check-libxml)
	(when (and (not version) (not (car gateway-version)))
		(gateway-set-version))
	;; TOOD: Let them choose when to jump to new buffer.
	;; (when (ignore-errors (gateway--assert-mode))
		;; (setq update (current-buffer)))
	(let* ((data (if (and (called-interactively-p) version)
							(gateway-fetch-version)
						(or version gateway-version)))
				 (version (car data))
				 (books (cdr data))
				 (passage-url (format "https://www.biblegateway.com/passage/?search=%s&version=%s&interface=print" passage version)))
		(with-current-buffer (url-retrieve-synchronously passage-url gateway-inhibit-cookies)
			(let* ((dom (libxml-parse-html-region (point) (point-max)))
						 (bcv (car (dom-strings (car (dom-by-class dom "^bcv$")))))
						 (translation (car (dom-strings (car (dom-by-class dom "^translation$")))))
						 (copyright (dom-by-class dom "^copyright-table$"))
						 (text (dom-by-class dom "^passage-text$"))
						 (prev (ignore-errors (dom-attr (car (dom-by-class dom "^prev-chapter$")) 'title)))
						 (next (ignore-errors (dom-attr (car (dom-by-class dom "^next-chapter$")) 'title)))
						 (passage-name (format "*BibleGateway: %s (%s)*" bcv version))
						 (struct `(:version ,data :text ,text :copyright ,copyright :bcv ,bcv :books ,books :translation ,translation :prev ,prev :next ,next)))
				(unless bcv (user-error (format "Could not find passage \"%s\" in version %s" passage version)))
				(if update (with-current-buffer update
							(gateway--assert-mode)
							(rename-buffer passage-name t)
							(set (make-local-variable 'gateway-data) struct)
							(gateway-refresh-passage))
					(with-output-to-temp-buffer passage-name
						(setq inhibit-read-only t)
						(pop-to-buffer passage-name)
						(gateway-display-mode)
						(set (make-local-variable 'gateway-data) struct)
						(gateway-refresh-passage)))))))

(define-derived-mode gateway-display-mode help-mode "Gateway"
	"Major mode for displaying Gateway passages."
	(setq gateway-display-mode-map (make-sparse-keymap))
  (set-keymap-parent gateway-display-mode-map help-mode-map)
	(define-key gateway-display-mode-map "C" #'gateway-toggle-crossrefs)
	(define-key gateway-display-mode-map "F" #'gateway-toggle-footnotes)
	(define-key gateway-display-mode-map "H" #'gateway-toggle-headings)
	(define-key gateway-display-mode-map "V" #'gateway-toggle-versenums)
	(define-key gateway-display-mode-map "P" #'gateway-display-verse-point)
	(use-local-map gateway-display-mode-map)
	(run-hooks 'gateway-display-mode-hook)
	;; :group gateway
	)

(defun gateway-toggle-crossrefs ()
	"Toggle the display of footnotes in the current BibleGateway buffer."
	(interactive)
	(gateway--assert-mode)
	(setq-local gateway-inhibit-crossrefs (not gateway-inhibit-crossrefs))
	(gateway--update-message "Cross-references"  gateway-inhibit-crossrefs)
	(message (format "Cross-references %sabled in current buffer" (if gateway-inhibit-crossrefs "dis" "en")))
	(gateway-refresh-passage))

(defun gateway-toggle-footnotes ()
	"Toggle the display of footnotes in the current BibleGateway buffer."
	(interactive)
	(gateway--assert-mode)
	(setq-local gateway-inhibit-footnotes (not gateway-inhibit-footnotes))
	(gateway--update-message "Footnotes" gateway-inhibit-footnotes)
	(gateway-refresh-passage))

(defun gateway-toggle-headings ()
	"Toggle the display of headings in the current BibleGateway buffer."
	(interactive)
	(gateway--assert-mode)
	(setq-local gateway-inhibit-headings (not gateway-inhibit-headings))
	(gateway--update-message "Headings" gateway-inhibit-headings)
	(gateway-refresh-passage))

(defun gateway-toggle-versenums ()
	"Toggle the display of verse numbers in the current BibleGateway buffer."
	(interactive)
	(gateway--assert-mode)
	(setq-local gateway-inhibit-versenums (not gateway-inhibit-versenums))
	(gateway--update-message "Verse numbers" gateway-inhibit-versenums)
	(gateway-refresh-passage))

(defun gateway-refresh-passage ()
	"Refresh the currently-displayed passage, applying the changes
to entity visibility settings."
	(interactive)
	(gateway--assert-mode)
	(let ((resilient (gateway--get-resilient-position)))
		(erase-buffer)
		(let ((text (plist-get gateway-data :text)))
			;; Set visibilities
			(gateway--refresh-entities (dom-by-class text (regexp-opt '("crossreference" "crossrefs"))) gateway-inhibit-crossrefs)
			(gateway--refresh-entities (dom-by-class text "footnote") gateway-inhibit-footnotes)
			(gateway--refresh-entities (dom-by-tag text 'h3) gateway-inhibit-headings)
			(gateway--refresh-entities (dom-by-class text (regexp-opt '("chapternum" "versenum"))) gateway-inhibit-versenums)

			;; Add advices
			(advice-add #'shr-tag-sup :around #'gateway--shr-tag-sup)
			(advice-add #'shr-tag-span :around #'gateway--shr-tag-span)
			(advice-add #'shr-tag-a :around #'gateway--shr-tag-a)
			(shr-insert-document text)
			(advice-remove #'shr-tag-sup #'gateway--shr-tag-sup)
			(advice-remove #'shr-tag-span #'gateway--shr-tag-span)
			(advice-remove #'shr-tag-a #'gateway--shr-tag-a))
		(plist-put gateway-data :end (1- (point)))
		(shr-insert-document '(html nil (body nil (hr nil))))
		(shr-insert-document (plist-get gateway-data :copyright))
		(apply #'gateway--restore-resilient-position resilient))
	(setq header-line-format (format " %s (%s)" (plist-get gateway-data :bcv) (plist-get gateway-data :translation))))

(defun gateway-find-verse (verse)
	"Find the verse with ID VERSE, and jump to its beginning as
given by `gateway-beginning-of-verse'."
	(let ((pos (point)))
		(goto-char (point-min))
		(while (not (string= (get-text-property (point) 'verse) verse))
			(when (or (>= (point) (plist-get gateway-data :end))
								(not (ignore-errors (gateway-right-verse))))
				(goto-char pos)
				(user-error (format "Verse ID \"%s\" not found in current selection" verse))))
		(gateway-beginning-of-verse)))

(defun gateway--get-resilient-position ()
	"Return a position in terms of spaces within the current verse,
which withstands changing the visibility of Scripture elements."
	(let* ((end (point))
				 (start (progn (gateway-beginning-of-verse) (point)))
				 (verse (gateway--verse-point))
				 (segment (buffer-substring start end)))
		(list verse)))

(defun gateway--restore-resilient-position (verse)
	"Restore a resilient position from `gateway--get-resilient-position'."
	(gateway-find-verse verse)
	;; (let ((end (get-text-property (point) 'verse)))
		;; (search-forward key)
		;; (when (>= (point) end) (user-error "Could not restore point"))))
	)

(defun gateway-get-verse-at-point (&optional format)
	"Return the reference of the current verse in human-readable
form when FORMAT is non-nil."
	(gateway--assert-mode)
	(let ((loc (get-text-property (point) 'verse)))
		(unless loc (user-error "No verse defined at point"))
		(if format
				(let* ((components (split-string loc "-")))
					(setf (car components) (gateway--resolve-osis (car components)))
					(apply #'format (push "%s %s:%s" components)))
			loc)))

(defun gateway-show-verse-at-point (&optional arg)
	"Display the reference of the current verse. To show the raw
tag ID, use a non-nil prefix argument."
	(interactive "P")
	(message (gateway-get-verse-at-point (not arg))))

(defun gateway--next-non-nil-single-char-property-change (position prop &optional object limit orig)
	(unless orig
		(setq orig (get-text-property position prop object)))
	(next-single-char-property-change position prop object limit)
	)

(defun gateway-beginning-of-verse (&optional no-align)
	"Move to the beginning of the current verse text. When NO-ALIGN
is non-nil, the point is set to the logical start of the verse,
including verse numbers or headings."
	(interactive "P")
	(gateway--assert-mode)
	(let ((pos (get-text-property (point) 'start)))
		(unless (get-text-property (point) 'verse)
			(if (<= (point) (plist-get gateway-data :end))
					(progn (backward-char) (gateway-beginning-of-verse))
				(user-error "No verse defined at point")))
		(when pos (goto-char pos))
		(while (and (not no-align) (get-text-property (point) 'class))
			(forward-char))))

(defun gateway-end-of-verse ()
	"Move to the end of the current verse."
	(interactive)
	(gateway--assert-mode)
	(let ((pos (get-text-property (point) 'end)))
		(unless (get-text-property (point) 'verse)
			(if (<= (point) (plist-get gateway-data :end))
					(progn (backward-char) (gateway-end-of-verse))
				(user-error "No verse defined at point")))
		(when pos (goto-char pos))))

(defun gateway-mark-verse ()
	"Mark the current verse."
	(interactive)
	(gateway--assert-mode)
		(gateway-beginning-of-verse)
		(set-mark (point))
		(gateway-end-of-verse)
		(activate-mark))

(defun gateway-left-verse ()
	"Move one verse to the left."
	(interactive)
	(gateway--assert-mode)
	(gateway-beginning-of-verse t)
	(backward-char)
	(gateway-beginning-of-verse)
	(point))

(defun gateway-right-verse ()
	"Move one verse to the right."
	(interactive)
	(gateway--assert-mode)
	(let ((curr (point)))
		(gateway-end-of-verse)
		(forward-char 2)
		(gateway-beginning-of-verse)
		(when (equal curr (point)) (user-error "No more verses"))
		(point)))

(defun gateway-left-chapter ()
	"Move one chapter to the left."
	(interactive)
	(gateway--assert-mode)
	(if (plist-get gateway-data :prev)
			(gateway-fetch-passage (plist-get gateway-data :prev) (plist-get gateway-data :version) (current-buffer))
		(user-error "No more chapters")))

(defun gateway-right-chapter ()
	"Move one chapter to the right."
	(interactive)
	(gateway--assert-mode)
	(if (plist-get gateway-data :next)
			(gateway-fetch-passage (plist-get gateway-data :next) (plist-get gateway-data :version) (current-buffer))
		(user-error "No more chapters")))

(provide 'gateway-mode)

;;; gateway-mode.el ends here
