;;; lsp-server.el --- Install LSP servers -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "6"))
;; Keywords: process tools
;; URL: https://github.com/akirak/lsp-server.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library helps you install language servers for lsp-mode.el.

;;; Code:

(require 'lsp-mode)
(require 'lsp-clients)
(require 'cl-lib)

(declare-function 'nix-env-install-npm "nix-env-install")

;;;; Custom variables

(defcustom lsp-server-package-alist
  '((bash-ls . (npm "bash-language-server"))
    (fsac . (function lsp-fsharp--fsac-install))
    (elixir-ls . (error "Please install elixir-ls for yourself")))
  "Alist of package specifications for servers."
  :group 'lsp-server
  :type '(alist :key-type symbol
                :value-type sexp))

(defcustom lsp-server-executable-check-skip-list
  '(fsac)
  "List of server IDs whose executable check should be skipped."
  :group 'lsp-server
  :type '(repeat symbol))

(defcustom lsp-server-browse-url-function #'browse-url
  "Function used to browse documentations of language servers."
  :group 'lsp-server
  :type 'function)

(defcustom lsp-server-use-nix (and (executable-find "nix-env") t)
  "Prefer installing packages using nix-env when applicable."
  :group 'lsp-server
  :type 'boolean)

;;;; Variables
(defvar lsp-server-library-index nil
  "Hashmap from server ids to library files where they are defined.")

;;;; Commands
;;;###autoload
(defun lsp-server-install (server-id)
  "Install the server package for SERVER-ID."
  (interactive (list (lsp-server--read-server-id "Choose a client: "
                                                 current-prefix-arg)))
  (if-let ((spec (alist-get server-id lsp-server-package-alist)))
      (lsp-server--install-with-server-spec spec)
    (lsp-server--discover-instruction server-id)))

;;;; Functions
(defun lsp-server--index-groups ()
  (message "Scanning LSP library files...")
  (setq lsp-server-library-index (make-hash-table :test #'eq))
  (thread-last
      (cl-merge 'list
                (lsp-server--builtin-files)
                (remq nil (mapcar (lambda (group)
                                    (ignore-errors (find-library-name (symbol-name group))))
                                  (lsp-server--get-groups)))
                #'file-equal-p)
    (mapc (lambda (file)
            (let* ((initial-buf (find-buffer-visiting file))
                   (buf (or initial-buf
                            (find-file-noselect file))))
              (unwind-protect
                  (with-current-buffer buf
                    (save-restriction
                      (save-excursion
                        (goto-char (point-min))
                        (while (re-search-forward (rx ":server-id"
                                                      (+ space)
                                                      "'"
                                                      (group (+ (any word "-_"))))
                                                  nil t)
                          (puthash (intern (match-string 1)) file lsp-server-library-index)))))
                (unless initial-buf
                  (kill-buffer buf))))))))

(defun lsp-server--builtin-files ()
  "Get a list of client definition libraries in lsp-mode package."
  (let ((dir (file-name-directory (find-library-name "lsp-mode"))))
    (thread-last (directory-files dir nil (rx "lsp-" (+ (any word "-")) ".el" eol))
      (funcall (lambda (files) (cl-remove "lsp-mode.el" files :test #'equal)))
      (cl-remove-if (lambda (filename) (string-suffix-p "-autoloads.el" filename)))
      (mapcar (lambda (filename) (expand-file-name filename dir))))))

(defun lsp-server--lookup-library-file (server-id)
  "Find a library file for SERVER-ID."
  ;; If the index is not created, scan libraries first.
  (unless lsp-server-library-index
    (lsp-server--index-groups))
  (gethash server-id lsp-server-library-index))

(defun lsp-server--read-server-id (prompt &optional all)
  "Read a server ID from the user.

This function lets the user pick a server ID via
`completing-read' interface.

PROMPT is a prompt in the `completing-read' interface.

If ALL is non-nil, all known server IDs are suggested.
Otherwise, servers applicable to the current buffer are suggested."
  (get-char-property 0 'lsp-server-id
                     (completing-read prompt (lsp-server--find-clients all))))

(defun lsp-server--find-clients (&optional all)
  "Return a list of LSP clients.

If ALL is non-nil, all known server IDs are suggested.
Otherwise, servers applicable to the current buffer are suggested."
  (thread-last (ht-values lsp-clients)
    (cl-remove-if-not (lambda (client)
                        (or all
                            (or (let ((fun (lsp--client-activation-fn client)))
                                  (and fun (funcall fun (buffer-file-name) major-mode)))
                                (memq major-mode (lsp--client-major-modes client))))))
    (mapcar #'lsp-server--format-client-candidate)))

(defun lsp-server--format-client-candidate (client)
  "Format a candidate for CLIENT for use in a `completing-read' interface.

This function provides a rich format for client candidates.
lsp-server-id property is added to the output."
  (let* ((server-id (lsp--client-server-id client))
         (info (lsp-server--get-client-info server-id))
         (group (plist-get info :custom-group)))
    (or (propertize (concat (format "%2d %s"
                                    (lsp--client-priority client)
                                    server-id)
                            (if group
                                (format " (%s): %s"
                                        group
                                        (propertize (or (get group 'group-documentation)
                                                        "")
                                                    'face 'font-lock-comment-face))
                              ""))
                    'lsp-server-id server-id)
        (symbol-name server-id))))

(defun lsp-server--get-client-info (server-id)
  "Retrieve information for SERVER-ID from Emacs Lisp source files."
  (let ((lsp-clients-file (or (lsp-server--lookup-library-file server-id)
                              (find-library-name "lsp-clients")))
        client-sexp
        group)
    (with-current-buffer (or (find-buffer-visiting lsp-clients-file)
                             (find-file-noselect lsp-clients-file))
      (save-restriction
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (concat ":server-id[[:space:]]+'"
                         (regexp-quote (symbol-name server-id)))
                 nil t)
            (beginning-of-defun)
            (setq client-sexp (read (thing-at-point 'sexp)))
            (when (re-search-backward (rx "(defgroup" (+ space) (group (+ (any word "-")))) nil t)
              (setq group (intern (match-string 1))))))))
    (list :executable (unless (memq server-id lsp-server-executable-check-skip-list)
                        (ignore-errors (lsp-server--extract-server-executable client-sexp)))
          :custom-group group)))

(defun lsp-server--discover-instruction (server-id)
  "Try to find an installation instruction for SERVER-ID."
  (let* ((plist (lsp-server--get-client-info server-id))
         (executable (plist-get plist :executable))
         (group (plist-get plist :custom-group)))
    (cond
     ;; Already installed
     ((and executable (executable-find executable))
      (message "%s is already installed: %s" executable
               (executable-find executable)))
     (executable
      (lsp-server--install-executable executable))
     (group
      (pcase (get group 'custom-links)
        ((or `((url-link ,url))
             (and `(,url) (guard (stringp url))))
         (when (yes-or-no-p (format "Failed to get an executable name. Browse %s?" url))
           (lsp-server--browse-url url)))))
     (t
      (error "No information on %s" server-id)))))

(defun lsp-server--browse-url (url)
  "Browse URL using `lsp-server-browse-url-function'."
  (funcall lsp-server-browse-url-function url))

(defun lsp-server--extract-server-executable (sexp)
  "Extract the name of a server executable from SEXP registering a client."
  (pcase-let* ((`(lsp-register-client ,client) sexp)
               (`(make-lsp-client . ,plist) client)
               (`(,connection-type ,command) (plist-get plist :new-connection))
               ((or `(cons ,executable ,_)
                    `(,executable . ,_)
                    executable)
                (cl-etypecase command
                  (string command)
                  (function (cl-ecase connection-type
                              (lsp-stdio-connection (funcall command))
                              (lsp-tcp-connection (funcall command 0))))
                  (list (pcase command
                          (`(lambda () ,program-and-args) program-and-args)
                          (_ (cl-typecase (eval command)
                               (function (cl-ecase connection-type
                                           (lsp-stdio-connection (funcall (eval command)))
                                           (lsp-tcp-connection (funcall (eval command) 0))))
                               (symbol (symbol-value (eval command)))
                               (otherwise (eval command))))))
                  (symbol (symbol-value command)))))
    (cl-etypecase executable
      (symbol (symbol-value executable))
      (string executable))))

(defun lsp-server--install-executable (executable)
  "Find an instruction for installing EXECUTABLE and execute it."
  (let* ((readme (expand-file-name "README.org"
                                   (file-name-directory
                                    (file-truename (find-library-name "lsp-mode")))))
         (org-buffer (find-buffer-visiting readme)))
    (if (file-exists-p readme)
        (unwind-protect
            (with-current-buffer (or org-buffer (find-file-noselect readme))
              (org-with-wide-buffer
               (goto-char (point-min))
               (re-search-forward (rx "*" (+ space) "Supported languages"))
               (re-search-forward org-table-line-regexp)
               (beginning-of-line)
               (pcase-let* ((result (catch 'result
                                      (let ((table-end (org-table-end)))
                                        (when (re-search-forward (regexp-quote executable) table-end t)
                                          (beginning-of-line)
                                          (throw 'result (org-element-table-row-parser (line-end-position)))))))
                            (`(_ _ _ ,command . ,_) (if result
                                                        (split-string (buffer-substring-no-properties
                                                                       (org-element-property :contents-begin result)
                                                                       (org-element-property :contents-end result))
                                                                      "|")
                                                      (error "%s is unfound in README" executable))))
                 (pcase command
                   ((pred (string-match (rx bol (+ space)
                                            "npm " (or "i" "install")
                                            (+ space) (or "-g" "--global")
                                            (+ space) (group (+ anything)))))
                    (let ((packages (split-string (match-string 1 command))))
                      (when (yes-or-no-p (format "Install %s from npm? " packages))
                        (lsp-server-install-npm-packages packages))))
                   (_ (when (yes-or-no-p (format "Run '%s' to install %s?"
                                                 command executable))
                        (compile command)))))))
          ;; Kill the buffer if it had not been open
          (unless org-buffer
            (kill-buffer org-buffer)))
      (error "README is not found"))))

;;;###autoload
(defun lsp-server-help-setup (group)
  "Browse documentation for GROUP."
  (interactive (list (intern (completing-read
                              "Group: "
                              (mapcar (lambda (cell) (symbol-name (car cell)))
                                      (lsp-server--get-groups-with-links))))))
  (pcase (alist-get group (lsp-server--get-groups-with-links))
    (url (lsp-server--browse-url url))
    (`(url-link ,url) (lsp-server--browse-url url))
    (_ (error "No link for %s" group))))

(defun lsp-server--get-groups ()
  "Get a list of customization groups under lsp-mode."
  (thread-last (get 'lsp-mode 'custom-group)
    (cl-remove-if-not (lambda (cell) (eq 'custom-group (nth 1 cell))))
    (mapcar #'car)))

(defun lsp-server--get-groups-with-links ()
  "Get an alist of customization groups under lsp-mode."
  (thread-last (lsp-server--get-groups)
    (mapcar (lambda (symbol)
              (let ((links (get symbol 'custom-links)))
                (when links
                  (cons symbol (car links))))))
    (remq nil)))

(defun lsp-server--install-with-server-spec (spec)
  "Install server packages from SPEC."
  (pcase spec
    (`(npm . ,packages)
     (if lsp-server-use-nix
         (progn
           (require 'nix-env-install)
           (nix-env-install-npm packages))
       (lsp-server--run-command (format "npm install -g %s"
                                        (mapconcat #'shell-quote-argument
                                                   packages ' ')))))
    (`(function ,func)
     (funcall func))
    (`(error ,msg)
     (message msg))
    (_
     (error "Unsupported spec %s" spec))))

(defun lsp-server--run-command (command)
  "Run COMMAND for installing packages."
  (if (yes-or-no-p (format "Run %s for installing packages? " COMMAND))
      (compile command)
    (user-error "Aborted")))

(provide 'lsp-server)
;;; lsp-server.el ends here
