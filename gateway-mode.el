;;; gateway-mode --- access Bible Gateway from Emacs

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

(defun gateway--get-versions ()
	"Get the static version information from BibleGateway, and
return an alist of the version abbreviation and full name."
	(gateway--check-libxml)
	(message "Retrieving version list from remote...")
	(with-current-buffer (url-retrieve-synchronously "https://biblegateway.com" t gateway-inhibit-cookies)
		(let* ((dom (libxml-parse-html-region (point) (point-max)))
					 (options (cddar (dom-by-tag dom 'select)))
					 (results nil))
			(dolist (option (reverse options) results)
				(unless (assoc 'class (cadr option))
					(push (cons (caddr option) (cdr (assoc 'value (cadr option)))) results))))))

(defun gateway-set-version ()
	"Set `gateway-version' with the valid options provided by BibleGateway."
	(interactive)
	(let* ((versions (gateway--get-versions))
				(version (completing-read "Version: " versions nil t)))
		(setq gateway-version (cdr (assoc version versions)))
		(message (format "Set BibleGateway version to %s" version))))

(defun gateway-get-passage (passage)
	"Load a PASSAGE from BibleGateway."
	(interactive "MReference: ") ; (format "MReference (%s): " gateway-version))
	(gateway--check-libxml)
	(unless gateway-version
		(gateway-set-version))
	(let ((passage-url (format "https://www.biblegateway.com/passage/?search=%s&version=%s&interface=print" passage gateway-version)))
		(with-current-buffer (url-retrieve-synchronously passage-url gateway-inhibit-cookies)
			(let* ((dom (libxml-parse-html-region (point) (point-max)))
						 (bcv (car (dom-strings (car (dom-by-class dom "^bcv$")))))
						 (translation (car (dom-strings (car (dom-by-class dom "^translation$")))))
						 (copyright (dom-by-class dom "^copyright-table$"))
						 (text (dom-by-class dom "^passage-text$"))
						 (passage-name (format "*BibleGateway: %s (%s)*" bcv gateway-version)))
				(unless bcv
					(user-error (format "Could not find passage \"%s\" in version %s" passage gateway-version)))
				(with-output-to-temp-buffer passage-name
					(setq inhibit-read-only t)
					(pop-to-buffer passage-name)
					(gateway-display-mode)
					(set (make-local-variable 'gateway-data) `(:text ,text :copyright ,copyright :bcv ,bcv :translation ,translation))
					(gateway-refresh-passage))))))

(defun gateway--assert-mode ()
	"Raise an error if the proper data structures are not resent."
	(unless (boundp 'gateway-data)
		(user-error "Buffer not in gateway-view-mode")))

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
	:group gateway)
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
	"Refresh the currently-displayed passage."
	(interactive)
	(gateway--assert-mode)

	;; TODO: Better preserve the point on heading change.
	(let ((pos (point)))
		(erase-buffer)
		(let ((text (copy-tree (plist-get gateway-data :text))))
			(when gateway-inhibit-crossrefs
				(dolist (node (dom-by-class text (regexp-opt '("crossreference" "crossrefs"))))
					(dom-remove-node text node)))
			(when gateway-inhibit-footnotes
				(dolist (node (dom-by-class text "footnote"))
					(dom-remove-node text node)))
			(when gateway-inhibit-headings
				(dolist (node (dom-by-tag text 'h3))
					(dom-remove-node text node)))
			(when gateway-inhibit-versenums
				(dolist (node (dom-by-class text (regexp-opt '("chapternum" "versenum"))))
					(dom-remove-node text node)))
			(advice-add #'shr-tag-span :around #'gateway--shr-tag-span)
			(shr-insert-document text)
			(advice-remove #'shr-tag-span #'gateway--shr-tag-span))
		(shr-insert-document '(html nil (body nil (hr nil))))
		(shr-insert-document (plist-get gateway-data :copyright))
		(goto-char pos))
	(setq header-line-format (format " %s (%s)" (plist-get gateway-data :bcv) (plist-get gateway-data :translation))))

(defun gateway--shr-tag-span (func &rest r)
	"Set the verse property for each span. Only intended to advise
`shr-tag-span'."
	(let* ((dom (car r))
				 (init (point))
				 (classes (split-string (cdr (assoc 'class (cadr dom))))))
		(apply func r)
		(when (string= (car classes) "text")
			(put-text-property init (point) 'verse (cadr classes)))))

(defun gateway--verse-point ()
	"Return the reference of the current verse."
	(gateway--assert-mode)
	(let ((loc (get-text-property (point) 'verse)))
		(unless loc
			(user-error "No verse defined at point"))
		(let* ((components (split-string loc "-")))
			(apply #'format (push "%s %s:%s" components)))))

(defun gateway-display-verse-point ()
	"Display the reference of the current verse."
	(interactive)
	(message (gateway--verse-point)))

(provide 'gateway-mode)
;;; gateway-mode.el ends here
