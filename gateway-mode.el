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

(defun gateway-set-version ()
	"Set the global `gateway-version' with the valid options
provided by BibleGateway."
	(interactive)
	(let ((version (cdr (gateway--version-completing-read))))
		(setq gateway-version version)
		(message (format "Set global BibleGateway version to %s" version))))

(defun gateway-change-version ()
	"Change the version for the current passage only."
	(interactive)
	(gateway--assert-mode)
	(let ((bcv (plist-get gateway-data :bcv))
				(version (cdr (gateway--version-completing-read))))
		(gateway-get-passage bcv version (buffer-name))
		(message "Changed local BibleGateway version to %s" version)))

(defun gateway-get-passage (passage &optional version update)
	"Load a PASSAGE from BibleGateway. The global version specified
in `gateway-version' is used, unless VERSION is provided. UPDATE
holds the name of an existing BibleGateway buffer to be updated,
which will be verified valid before writing."
	(interactive "MReference: ") ; (format "MReference (%s): " gateway-version))
	(gateway--check-libxml)
	(unless gateway-version
		(gateway-set-version))
	(let* ((version (or version gateway-version))
				 (passage-url (format "https://www.biblegateway.com/passage/?search=%s&version=%s&interface=print" passage version)))
		(with-current-buffer (url-retrieve-synchronously passage-url gateway-inhibit-cookies)
			(let* ((dom (libxml-parse-html-region (point) (point-max)))
						 (bcv (car (dom-strings (car (dom-by-class dom "^bcv$")))))
						 (translation (car (dom-strings (car (dom-by-class dom "^translation$")))))
						 (copyright (dom-by-class dom "^copyright-table$"))
						 (text (dom-by-class dom "^passage-text$"))
						 (passage-name (format "*BibleGateway: %s (%s)*" bcv version)))
				(unless bcv (user-error (format "Could not find passage \"%s\" in version %s" passage version)))
				(if update (with-current-buffer update
							(gateway--assert-mode)
							(rename-buffer passage-name)
							(set (make-local-variable 'gateway-data) `(:text ,text :copyright ,copyright :bcv ,bcv :translation ,translation))
							(gateway-refresh-passage))
					(with-output-to-temp-buffer passage-name
						(setq inhibit-read-only t)
						(pop-to-buffer passage-name)
						(gateway-display-mode)
						(set (make-local-variable 'gateway-data) `(:text ,text :copyright ,copyright :bcv ,bcv :translation ,translation))
						(gateway-refresh-passage)))))))

(defun gateway--assert-mode ()
	"Raise an error if the proper data structures are not resent."
	(unless (boundp 'gateway-data)
		(user-error "Not a BibleGateway buffer")))

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

(defun gateway--update-message (entity status)
	"Display a message when a visibility ENTITY is set to STATUS."
	(message (format "%s %sabled in current buffer" entity (if status "dis" "en"))))

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

	(let ((pos (point)))
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
		(goto-char pos))
	(setq header-line-format (format " %s (%s)" (plist-get gateway-data :bcv) (plist-get gateway-data :translation))))

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

(defun gateway--verse-point ()
	"Return the reference of the current verse."
	(gateway--assert-mode)
	(let ((loc (get-text-property (point) 'verse)))
		(unless loc (user-error "No verse defined at point"))
		(let* ((components (split-string loc "-")))
			(apply #'format (push "%s %s:%s" components)))))

(defun gateway-display-verse-point ()
	"Display the reference of the current verse."
	(interactive)
	(message (gateway--verse-point)))

(defun gateway-beginning-of-verse (&optional no-align)
	"Move to the beginning of the current verse text. When NO-ALIGN
is non-nil, the point is set to the logical start of the verse,
including verse numbers or headings."
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
	(let ((pos (get-text-property (point) 'end)))
		(unless (get-text-property (point) 'verse)
			(if (<= (point) (plist-get gateway-data :end))
					(progn (backward-char) (gateway-end-of-verse))
				(user-error "No verse defined at point")))
		(when pos (goto-char pos))))

(defun gateway-left-verse ()
	"Move one verse to the left."
	(gateway-beginning-of-verse t)
	(backward-char)
	(gateway-beginning-of-verse))

(defun gateway-right-verse ()
	"Move one verse to the right."
	(gateway-end-of-verse)
	(forward-char 2)
	(gateway-beginning-of-verse))

(provide 'gateway-mode)
;;; gateway-mode.el ends here
