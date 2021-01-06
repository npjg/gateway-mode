;;; gateway-mode --- access Bible Gateway from Emacs

;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; URL: http://github.com/npjg/gateway-mode
;; Package-Requires:
;; Version: 0.1
;; Created: 2020-01-05
;; By: Nathanael Gentry <nathanael.gentrydb8@gmail.com>
;; Keywords: Bible

(defgroup gateway-mode 'nil
	"Customization group for gateway-mode")

(defvar gateway-version nil
	"The Bible version to use for interacting with Bible Gateway.
To modify with available versions, use `gateway-set-version'.")

(defvar gateway-inhibit-cookies t
	"Whether or not to inhibit cookies in BibleGateway URL requests.")

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
					(pop-to-buffer passage-name)
					(setq header-line-format (format " %s (%s)" bcv translation))
					(shr-insert-document text)
					(shr-insert-document '(html nil (body nil (hr nil))))
					(shr-insert-document copyright))
			))))

(provide 'gateway-mode)
;;; gateway-mode.el ends here
