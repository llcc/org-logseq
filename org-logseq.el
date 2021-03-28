;;; org-logseq.el --- for logseq  -*- lexical-binding: t; -*-

;; Author: Zhe Lei <lzhes43@gmail.com>
;; URL: https://github.com/llcc/org-logseq
;; Package-Version: 20210327.2210
;; Version: 0.0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make logseq work easier in Emacs

;;; Code:

(require 'ol)

(defgroup org-logseq nil
  "Logseq capbility in Org Mode"
  :group 'org)

(defcustom org-logseq-dir nil
  "Path of logseq notes"
  :group 'org-logseq)

(defcustom org-logseq-create-page-p nil
  "Non-nil means Org-logseq creates a new page if it does not exist."
  :group 'org-logseq)

(defun org-logseq-get-id (&optional beg end)
  "Return a cons: \"('id . id)\" at point."
  (save-excursion
    (when-let* ((prev-bracket (or beg (search-backward-regexp
                                       "((" (line-beginning-position) t))))
      (let* ((next-bracket (or end (search-forward-regexp
                                 "))" (line-end-position) t)))
             (id (buffer-substring-no-properties
                  (+ prev-bracket 2) (- next-bracket 2))))
        (cons 'id id)))))

(defun org-logseq-get-path ()
  "Return a cons: \"('type . path)\" at point.
The type can be 'url, 'draw and 'page, denoting the path type."
  (save-excursion
    (let ((context (org-element-context))
          path)
      (when (eq 'link (car context))
        (setq path (org-element-property :raw-link context))
        (cond ((string-match "\\(?:https?\\)" path)
               (cons 'url path))
              ((string-suffix-p ".excalidraw" path)
               (cons 'draw path))
              (t (cons 'page path)))))))

(defun org-logseq-grep-query (page-or-id)
  "Return grep result for searching PAGE-OR-ID in `org-logseq-dir'."
  (let ((type (car page-or-id))
        (query (cdr page-or-id)))
    (format (pcase type
              ('page "grep -niR \"^#+\\(TITLE\\): *%s\" \"%s\" --exclude-dir=\".git\"" )
              ('id "grep -niR \"^:id: *%s\" \"%s\" --exclude-dir=\".git\""))
            query org-logseq-dir)))

(defun org-logseq-create-page (page)
  "Create a new PAGE org file in pages directory if setting
 `org-logseq-create-page-p' to non-nil."
  (if org-logseq-create-page-p
      (find-file (expand-file-name (concat "pages/" page) github-dir))
    (user-error "No page found; Check `org-logseq-create-page-p` variable")))

;;;###autoload
(defun org-logseq-open-link ()
  "Open link at point. Supports url, id and page."
  (interactive)
  (when-let* ((t-l (or (org-logseq-get-path)
                       (org-logseq-get-id))))
    (pcase (car t-l)
      ('url (browse-url (cdr t-l)))
      ('draw (org-logseq-open-draw (cdr t-l)))
      (_ (let ((result (shell-command-to-string (org-logseq-grep-query t-l))))
           (if (string= result "")
               (org-logseq-create-page (cdr t-l))
             (let* ((f-n (split-string result ":" nil))
                    (fname (car f-n))
                    (lineno (string-to-number (cadr f-n))))
               (org-open-file fname t lineno))))))))

;; todo: looking for a way to open draw file by post
(defun org-logseq-open-draw nil)

(defvar org-logseq-excalidraw
  "{
  \"type\": \"excalidraw\",
  \"version\": 2,
  \"source\": \"https://excalidraw.com\",
  \"elements\": [],
  \"appState\": {
    \"gridSize\": null,
    \"viewBackgroundColor\": \"#ffffff\"
  }
}"
  "Blank excalidraw file")

(defun org-logseq-new-excalidraw (&optional name)
  "Create an excalidraw file and insert it at point."
  (interactive "P")
  (let ((draw-fname (format "%s.excalidraw"
                            (or name (format-time-string "%Y-%m-%d_%H_%M_%S")))))
    (insert (format "[[draws/%s]]" draw-fname))
    (with-current-buffer
        (find-file-noselect (expand-file-name (format "draws/%s" draw-fname)
                                              org-logseq-dir))
      (insert org-logseq-excalidraw)
      (save-buffer))))

;;;###autoload
(defun org-logseq-contents-sidebar ()
  "Display contents.org as sidebar left side."
  (interactive)
  (display-buffer-in-side-window
   (find-file (expand-file-name "pages/contents.org" org-logseq-dir))
   (list (cons 'side 'left)
         (cons 'window-width 30))))

;; Logseq id overlay
(defvar-local org-logseq-id-overlays nil)

(defun org-logseq-id-heading (id)
  (when-let ((result (shell-command-to-string
                      (org-logseq-grep-query id))))
    (let* ((f-n (split-string result ":" nil))
           (fname (car f-n))
           (lineno (string-to-number (cadr f-n))))
      (with-current-buffer (find-file-noselect fname)
        (goto-line lineno)
        (org-no-properties (org-get-heading))))))

(defun olih-link (heading)
  (if (string-match "\\[\\[.+\\]\\[\\(.+\\)\\]\\]" heading)
      (match-string 1 heading)
    heading))

(defcustom org-logseq-id-heading-hook
  '(olih-link)
  "Hook for cleaning up id heading")

(defun org-logseq-create-id-overlay (beg end)
  (let* ((ov (make-overlay beg end))
         (id (org-logseq-get-id beg end))
         (heading (let ((result (org-logseq-id-heading id)))
                    (dolist (func org-logseq-id-heading-hook) 
                      (setq result (funcall func result)))
                    result)))
    (overlay-put ov 'logseq-id t)
    (overlay-put ov 'display heading)
    (overlay-put
	 ov 'modification-hooks
	 (list 'org-logseq-remove-id-overlays))
    (push ov org-logseq-id-overlays)))

(defun org-logseq-remove-id-overlays ()
  (interactive)
  (let ((inhibit-modification-hooks t))
    (mapc #'delete-overlay org-logseq-id-overlays)
    (setq org-logseq-id-overlays nil)))

(defun org-logseq-toggle-id ()
  (interactive)
  (if org-logseq-id-overlays
      (org-logseq-remove-id-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "((.+))" nil t)
        (let ((end (point))
              (beg (re-search-backward "((" (line-beginning-position) t)))
          (org-logseq-create-id-overlay beg end)
          (goto-char end))))))

(defcustom org-logseq-display-id-p nil
  "Boolean value to display id overlay or not")

(add-hook 'org-mode-hook
          (lambda () (when org-logseq-display-id-p
                       (call-interactively 'org-logseq-toggle-id))))

(defvar org-logseq-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-open-at-point] 'org-logseq-open-link)
    (define-key map [remap org-mouse-down-mouse] 'org-logseq-open-link)
    map)
  "Org-logseq map")

(define-minor-mode org-logseq-mode
  "Org-logseq minor mode"
  :init-value nil
  :keymap org-logseq-map)

(provide 'org-logseq)
;;; org-logseq.el ends here
