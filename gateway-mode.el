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
	To modify with available versions, use `gateway-set-version'")

(defvar gateway-inhibit-cookies t
	"Whether or not to inhibit cookies in BibleGateway URL requests.")

(defun gateway--get-versions ()
	"Get the static version information from BibleGateway, and
	return an alist of the version abbreviation and full name."
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
	(let ((versions (gateway--get-versions)))
		(setq gateway-version (cdr (assoc (completing-read "Version: " versions nil t) versions)))))

(provide 'gateway-mode)
;;; gateway-mode.el ends here
