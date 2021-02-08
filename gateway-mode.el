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

(defvar gateway-inhibit-crossrefs t
	"If non-nil, inhibit BibleGateway cross-references.")

(defvar gateway-inhibit-footnotes nil
	"If non-nil, inhibit BibleGateway footnotes.")

(defvar gateway-inhibit-headings nil
	"If non-nil, inhibit BibleGateway headings.")

(defvar gateway-inhibit-versenums nil
	"If non-nil, inhibit BibleGateway verse numbers.")

(defvar gateway-inhibit-woJ nil
	"If non-nil, inhibit BibleGateway WoJ display.

When nil, give WoJ the foreground colour specified in `gateway-woj-color'")

(defvar gateway-woJ-color nil
	"If non-nil, specify the foreground color for WoJ.")

(defvar gateway-display-mode-hook nil
  "Normal hook run when entering gateway-display-mode.")

(defvar gateway-max-passage-message-chars 2000
	"If non-nil, specifies the maximum number of verse characters to
display in a minibuffer message.")

(defvar gateway-reuse-same-buffer-action #'display-buffer-reuse-window
	"If non-nil, perform passage lookups in an existing
	BibleGateway buffer, passing the specified function as the
	action to `display-buffer'. A new buffer is created if it does
	not exist.")

(defvar gateway-show-headerline t
	"If non-nil, set the frozen header line to the current passage
	and version. When nil, use no header line.")

(defconst gateway-default-keys
	'(("C" gateway-toggle-crossrefs)
		("F" gateway-toggle-footnotes)
		("H" gateway-toggle-headings)
		("V" gateway-toggle-versenums)
		("P" gateway-show-verse-at-point)
		("J" gateway-left-chapter)
		("j" gateway-left-verse)
		("K" gateway-right-chapter)
		("k" gateway-right-verse)
		("g" gateway-refresh-passage))
		"The default keymap for `gateway-display-mode'.")

(defun gateway--check-libxml ()
	"Assert that libxml2 is compiled into Emacs."
	(unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2")))

(defun gateway--assert-mode ()
	"Raise an error if the proper data structures are not present,
and return t otherwise."
	(if (boundp 'gateway-data) t
		(error "Not a BibleGateway buffer")))

(defun gateway--update-message (entity status)
	"Display a message when a visibility ENTITY is set to STATUS."
	(message (format "%s %sabled in current buffer" entity (if status "dis" "en"))))

(defun gateway--format-reference ()
	"Format a reference for the current BibleGateway buffer."
	(gateway--assert-mode)
	(format "%s (%s)" (plist-get gateway-data :bcv) (plist-get gateway-data :translation)))

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

(defun gateway--verse-id-p (id)
	"Returns non-nil if ID is a valid verse ID (osis), nil otherwise."
	(let ((comps (split-string id "-")))
		(and (= (length comps) 3)
				 (> (string-to-number (cadr comps)) 0)
				 (> (string-to-number (caddr comps)) 0))))

(defun gateway--shr-tag-span (func &rest r)
	"Set the verse property for each span, and also mark chapter
numbers and poetry indentation. Only intended to advise
`shr-tag-span'."
	(let* ((dom (car r))
				 (init (point))
				 (id (cdr (assoc 'id (cadr dom))))
				 (classes (split-string (cdr (assoc 'class (cadr dom))))))
		;; We are not loading CSS, so fontize the chapter number and WoJ
		;; automatically.
		(if (string= (car classes) "chapternum")
				(progn (funcall #'shr-fontize-dom dom 'bold)
							 (put-text-property init (point) 'class (car classes)))
			(apply func r))
		;; TODO: Use a full face.
		(if (and (string= (car classes) "woj") gateway-woJ-color)
				(shr-colorize-region init (point) gateway-woJ-color))
		(when (and (= (length classes) 2) (not (string= (cadr classes) last-verse)))
			(put-text-property init (1+ init) 'verse (cadr classes))
			(put-text-property init (1+ init) 'id id)
			(setq last-verse (cadr classes)))))

(defun gateway--shr-tag-sup (func &rest r)
	"Set the class for each sup. Only intended to advise
`shr-tag-sup'."
		(let* ((dom (car r))
					 (init (point))
					 (class (cdr (assoc 'class (cadr dom)))))
			(apply func r)
			(put-text-property init (point) 'class class)))

(defun gateway--shr-tag-h3 (func &rest r)
	"Set the heading class for each h3. Only intended to advise
`shr-tag-h3'."
	(let* ((dom (car r))
				 (init (point)))
		(apply func r)
		(put-text-property init (point) 'class 'heading)))

(defun gateway--shr-tag-h4 (func &rest r)
	"Set the heading class for each h4. Only intended to advise
`shr-tag-h3'."
	(let* ((dom (car r))
				 (init (point)))
		(apply func r)
		(shr-add-font init (point) 'bold-italic)))

(defun gateway--shr-tag-a (func &rest r)
	"Set the footnote tooltip text to the actual value of the
footnote. Only intended to advise `shr-tag-a'."
	(let* ((dom (car r))
				 (init (point))
				 (id (cdr (assoc 'href (cadr dom)))))
		(apply func r)
		(cond ((string-match "^#[fc]..-" id)
					 (let* ((text (plist-get gateway-data :text))
									(rendered (with-temp-buffer (shr-descend (dom-by-id text (substring id 1))) (buffer-substring (point-min) (point-max)))))
						 (put-text-property init (point) 'help-echo rendered)
						 (put-text-property init (point) 'keymap nil)
						 (put-text-property init (point) 'follow-link nil)))
					((string-match "^#..-" id))
					((string-match "^/" id)
					 (let* ((relative (get-text-property (1- (point)) 'shr-url)))
						 (when relative (put-text-property init (1- (point)) 'shr-url (format "https://biblegateway.com%s" relative))))))))

(defun gateway--resolve-osis (osis)
	"Convert standard book abbrevation OSIS to human-friendly form."
	(gateway--assert-mode)
	(catch 'found
		(mapc (lambda (book) (if (string= (alist-get 'osis book) osis) (throw 'found (alist-get 'display book))))
					(plist-get gateway-data :books))
		nil))

(defun gateway--set-first-verse-id ()
	"In a quirk of the returned HTML, the first verse span returned
from a query does not have a verse ID attached to it. The ID is
contained only in the header. If there is more than 1 verse
loaded, this function subtracts 1 from the second verse's ID to
obtain the proper ID for the first verse."
	(gateway--assert-mode)
	(save-excursion
		(goto-char (point-min))
		(when (ignore-errors (gateway-end-of-verse))
			(gateway-right-verse t t)
			(let* ((nextid (split-string (get-text-property (point) 'id) "-"))
						 (thisid (format "%s-%s-%d" (car nextid) (cadr nextid)
														 (1- (string-to-number (caddr nextid))))))
				(gateway-left-verse t t)
				(put-text-property (point) (1+ (point)) 'id thisid))))
	nil)

(defun gateway-get-version ()
	"Get the global `gateway-get-version', and set it if it is not
fully set."
	(unless gateway-version (gateway-set-version))
	(when (stringp gateway-version)
		(setq gateway-version (cons gateway-version (gateway-fetch-bcv gateway-version))))
	gateway-version)

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
	(unless version (gateway-get-version))
	(with-current-buffer
			(url-retrieve-synchronously
			 (format "https://www.biblegateway.com/votd/get/?format=json&version=%s"
							 (or version (car gateway-version))))
		(goto-char url-http-end-of-headers)
		(let* ((json-object-type 'alist)
					 (json-key-type 'symbol)
					 (json-array-type 'list)
					 (votd (cdar (json-read)))
					 (display-dom (with-temp-buffer
							(insert (cdr (assoc 'content votd)))
							(libxml-parse-html-region (point-min) (point-max)))))
			(when (assoc 'code votd)
				(user-error (format "BibleGateway: (%s) %s" (cdr (assoc 'code votd)) (cdr (assoc 'message votd)))))
			(message (format "%s (%s; %s)"
											 (progn (with-temp-buffer (shr-descend display-dom) (buffer-substring (point-min) (point-max))))
											 (cdr (assoc 'display_ref votd))
											 (cdr (assoc 'version_id votd)))))))

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
will be verified valid before writing. Providing UPDATE
overriides the global `gateway-reuse-same-buffer' setting."
	(interactive "MReference: \nP")
	(gateway--check-libxml)
	(unless version (gateway-get-version))
	(when (and gateway-reuse-same-buffer-action (not update))
		(setq update (catch 'found (dolist (buffer (buffer-list) nil)
				(when (with-current-buffer buffer (ignore-errors (gateway--assert-mode)))
					(throw 'found buffer))))))
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
				(dolist (class '("full-chap-link" "passage-other-trans"))
					(dom-remove-node dom (car (dom-by-class dom class))))
				(unless bcv (user-error (format "Could not find passage \"%s\" in version %s" passage version)))
				(cond ((bufferp update) (with-current-buffer update
								 (gateway--assert-mode)
								 (rename-buffer passage-name t)
								 (set (make-local-variable 'gateway-data) struct)
								 (gateway-refresh-passage t)
								 (display-buffer update gateway-reuse-same-buffer-action)))
							((eq update 'string) (with-temp-buffer
								 (setq inhibit-read-only t)
								 (gateway-display-mode)
								 (set (make-local-variable 'gateway-data) struct)
								 (gateway-refresh-passage t)
								 (let ((result (concat (buffer-substring (point-min) (plist-get gateway-data :end))
																			 "\n\n -- " (gateway--format-reference))))
									 (kill-buffer)
									 result)))
							(t (with-output-to-temp-buffer passage-name
									 (setq inhibit-read-only t)
									 (pop-to-buffer passage-name)
									 (gateway-display-mode)
									 (set (make-local-variable 'gateway-data) struct)
									 (gateway-refresh-passage t))))))))

(defun gateway-fetch-passage-to-minibuffer (passage &optional version)
	(interactive "MReference: \nP")
	(let ((passage (gateway-fetch-passage passage version 'string)))
		(if (or (not gateway-max-passage-message-chars) (< (length passage) gateway-max-passage-message-chars))
				(message passage)
			(user-error "Cannot display passage in minibuffer; maximum character limit reached"))))

(define-derived-mode gateway-display-mode help-mode "Gateway"
	"Major mode for displaying Gateway passages."
	(setq gateway-display-mode-map (make-sparse-keymap "Gateway"))
  (set-keymap-parent gateway-display-mode-map help-mode-map)
	(dolist (key gateway-default-keys)
		(apply #'define-key gateway-display-mode-map key))
	(use-local-map gateway-display-mode-map)
	(run-hooks 'gateway-display-mode-hook)
	;; :group gateway
	)

(defmacro define-gateway-toggle (entity)
	"Declare a toggler for various features of the BibleGateway display."
	`(defun ,(intern (format "gateway-toggle-%s" entity)) (&optional arg)
		 (format "Toggle the display of %s in the current BibleGateway buffer." ,entity)
		 (interactive)
		 (gateway--assert-mode)
		 (setq-local ,(intern (format "gateway-inhibit-%s" entity))
								 (if arg (> arg 0) (not ,(intern (format "gateway-inhibit-%s" entity)))))
		 (gateway--update-message ,(concat (capitalize (substring entity 0 1)) (substring entity 1))
															,(intern (format "gateway-inhibit-%s" entity)))
		 (gateway-refresh-passage)))

(define-gateway-toggle "crossrefs")
(define-gateway-toggle "headings")
(define-gateway-toggle "footnotes")
(define-gateway-toggle "versenums")
(define-gateway-toggle "woJ")

(defun gateway-refresh-passage (&optional new)
	"Refresh the currently-displayed passage, applying the changes
to entity visibility settings. When NEW is non-nil, the
function does not apply resilient position matching and instead
jumps to the beginning of the buffer."
	(interactive)
	(gateway--assert-mode)
	(let ((resilient (unless new (ignore-errors (gateway-get-resilient-position)))))
		(erase-buffer)
		(let ((text (plist-get gateway-data :text))
					(last-verse nil)
					(advices '("sup" "span" "a" "h3" "h4")))
			;; Set visibilities
			(gateway--refresh-entities (dom-by-class text (regexp-opt '("crossreference" "crossrefs"))) gateway-inhibit-crossrefs)
			(gateway--refresh-entities (dom-by-class text "footnote") gateway-inhibit-footnotes)
			(gateway--refresh-entities (dom-by-tag text 'h3) gateway-inhibit-headings)
			(gateway--refresh-entities (dom-by-class text (regexp-opt '("chapternum" "versenum"))) gateway-inhibit-versenums)

			;; Add advices
			(dolist (node advices nil)
				(advice-add (intern (format "shr-tag-%s" node)) :around
										(intern (format "gateway--shr-tag-%s" node))))
			(shr-insert-document text)
			(dolist (node advices nil)
				(advice-remove (intern (format "shr-tag-%s" node))
											 (intern (format "gateway--shr-tag-%s" node)))))
		(plist-put gateway-data :end (- (point) 2))
		(shr-insert-document '(html nil (body nil (hr nil))))
		(shr-insert-document (plist-get gateway-data :copyright))
		(gateway--set-first-verse-id)
		(if resilient (gateway-restore-resilient-position resilient)
			(goto-char (point-min))
			(gateway-beginning-of-verse nil t)
			(unless new (message "Could not restore point after refresh"))))
	(when gateway-show-headerline (setq header-line-format (concat " " (gateway--format-reference)))))

(defun gateway-get-resilient-position ()
	"Return a cons cell containing the current verse in the `car'
	and the plain character index in the `cdr'. Plain character
	index does not include any superscripts like verse numbers,
	footnotes, or cross-references."
	(let* ((start (point))
				 (verse (gateway-get-verse-at-point))
				 (plain-chars 0))
		(save-excursion
			(gateway-beginning-of-verse nil t)
			(while (<= (point) start)
				(unless (get-text-property (point) 'class)
					(setq plain-chars (1+ plain-chars)))
				(right-char)))
		(cons verse plain-chars)))

(defun gateway-restore-resilient-position (spec)
	"Use the SPEC from `gateway-get-resilient-position' to
restore the point."
	(gateway-find-verse (car spec))
	(let ((plain-chars (cdr spec)))
		(while (> plain-chars 0)
			(unless (get-text-property (point) 'class)
				(setq plain-chars (1- plain-chars)))
			(right-char)))
	(if (<= (point) (save-excursion (gateway-beginning-of-verse nil t) (point)))
			(gateway-beginning-of-verse nil t)
		(left-char)))

(defun gateway-find-verse (verse &optional actual-start)
	"Find the verse with ID VERSE, and jump to its beginning as
given by `gateway-beginning-of-verse', using ACTUAL-START as it
is described in `gateway-beginning-of-verse'."
	(let ((pos (point))
				(compare (if (string-match "[a-z]*-[A-Z]*-[0-9]*" verse) 'id 'verse)))
		(goto-char (point-min))
		(while (not (string= (get-text-property (point) compare) verse))
			(when (or (>= (point) (plist-get gateway-data :end))
								(not (ignore-errors (gateway-right-verse t t))))
				(goto-char pos)
				(user-error (format "Verse ID \"%s\" not found in current selection" verse))))
		(gateway-beginning-of-verse actual-start t)))

(defun gateway-get-verse-at-point (&optional format)
	"Return the reference of the current verse in human-readable
form when FORMAT is non-nil.

When FORMAT is :biblehub, format the verse in accord with
BibleHub's URL scheme."
	(gateway--assert-mode)
	(save-excursion
		(gateway-beginning-of-verse t t)
		(let ((loc (get-text-property (point) 'verse)))
			(unless loc (user-error "No verse defined at point"))
			(if format
					(let* ((components (split-string loc "-")))
						(setf (car components) (gateway--resolve-osis (car components)))
						(apply #'format (push (if (eq format :biblehub) "%s/%s-%s" "%s %s:%s") components)))
				loc))))

(defun gateway-show-verse-at-point (&optional arg loc)
	"Display the reference of the current verse. To show the raw
tag ID, use a non-nil prefix argument."
	(interactive "P")
	(save-excursion (goto-char (or loc (point))) (message (gateway-get-verse-at-point (not arg)))))

(defun gateway--position-point (forward)
	"Position the point at the beginning of the current verse, or
the end of the current verse if FORWARD is non-nil."
	(gateway--assert-mode)
	(when (> (point) (plist-get gateway-data :end))
		(user-error "No verse defined at point"))
	(let ((start (point)))
		(while (or (not (get-text-property (point) 'verse)) (= start (point)))
			(if forward (right-char) (left-char))
			(when (> (point) (plist-get gateway-data :end)) (user-error "No more verses")))))

(defun gateway-beginning-of-verse (&optional actual-start no-message)
	"Move to the beginning of the current verse text. When
ACTUAL-START is non-nil, the point is set to the real start of
the verse, including verse numbers, footnotes, cross-references,
and headings. Otherwise, the point is set to the first word of
the verse."
	(interactive "P")
	(gateway--assert-mode)
	(unless (get-text-property (point) 'verse)
		(gateway--position-point nil))
	(unless actual-start
		(while (and (<= (point) (plist-get gateway-data :end))
								(get-text-property (point) 'class))
			(goto-char (next-single-char-property-change (point) 'class))))
	(unless no-message (message (gateway-get-verse-at-point t))) t)

(defun gateway-end-of-verse (&optional no-message)
	"Move to the end of the current verse."
	(interactive)
	(gateway--assert-mode)
	(unless (get-text-property (1+ (point)) 'verse)
		(gateway--position-point t)
		(left-char))
	(unless no-message
		(message (gateway-get-verse-at-point nil))))

(defun gateway-mark-verse (&optional actual-start)
	"Mark the current verse, using ACTUAL-START as it is described
in `gateway-beginning-of-verse'."
	(interactive "P")
	(gateway--assert-mode)
	(gateway-beginning-of-verse actual-start)
	(set-mark (point))
	(gateway-end-of-verse)
	(activate-mark))

(defun gateway-left-verse (&optional actual-start no-message)
	"Move one verse to the left. using ACTUAL-START as it is described
in `gateway-beginning-of-verse'."
	(interactive "P")
	(gateway--assert-mode)
	(let ((past (>= (point) (plist-get gateway-data :end))))
	(when past (goto-char (plist-get gateway-data :end)))
	(gateway-beginning-of-verse t t)
	(unless (or past (= (point) (point-min)))
		(left-char)))
	(gateway-beginning-of-verse actual-start no-message))

(defun gateway-right-verse (&optional actual-start no-message)
	"Move one verse to the right. using ACTUAL-START as it is
described in `gateway-beginning-of-verse'."
	(interactive "P")
	(gateway--assert-mode)
	(ignore-errors (gateway--position-point t))
	(when (>= (point) (plist-get gateway-data :end))
				(goto-char (plist-get gateway-data :end)))
	(gateway-beginning-of-verse actual-start no-message))

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
