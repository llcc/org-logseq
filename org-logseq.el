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

(defcustom org-logseq-new-page-p nil
  "Non-nil means new a page if not exist."
  :group 'org-logseq)

(defun org-logseq-get-id (&optional beg end embed)
  "Return a cons: \"('id . id)\" at point."
  (save-excursion
    (when-let* ((prev-bracket (or beg (search-backward-regexp
                                       "((" (line-beginning-position) t))))
      (let* ((next-bracket (or end (search-forward-regexp
                                    "))" (line-end-position) t)))
             (id (buffer-substring-no-properties
                  (+ prev-bracket 2) (- next-bracket 2))))
        (cons 'id id)))))

(defun org-logseq-get-link ()
  "Return a cons: \"('type . link)\" at point.
The type can be 'url, 'draw and 'page, denoting the link type."
  (save-excursion
    (let ((context (org-element-context)) link)
      (when (eq 'link (car context))
        (setq link (org-element-property :raw-link context))
        (cond ((string-match "\\(?:https?\\)" link)
               (cons 'url link))
              ((string-suffix-p ".excalidraw" link)
               (cons 'draw link))
              (t (cons 'page link)))))))

(defun org-logseq-grep-query (page-or-id)
  "Return grep result for searching PAGE-OR-ID in `org-logseq-dir'."
  (let ((type (car page-or-id))
        (query (cdr page-or-id)))
    (format (pcase type
              ('page "grep -niR \"^#+\\(TITLE\\): *%s\" \"%s\" --exclude-dir=\".git\"" )
              ('id "grep -niR \":id: *%s\" \"%s\" --exclude-dir=\".git\""))
            query org-logseq-dir)))

;;;###autoload
(defun org-logseq-open-link ()
  "Open link at point. Supports url, id and page."
  (interactive)
  (when-let* ((t-l (or (org-logseq-get-link)
                       (org-logseq-get-id))))
    (let ((type (car t-l))
          (link (cdr t-l)))
      (pcase type
        ('url (browse-url link))
        ('draw (org-logseq-open-excalidraw link))
        (_ (let ((result (shell-command-to-string
                          (org-logseq-grep-query t-l))))
             (if (string= result "")
                 (org-logseq-new-page link)
               (let* ((f-n (split-string result ":" nil))
                      (fname (car f-n))
                      (lineno (string-to-number (cadr f-n))))
                 (delete-other-windows)
                 (org-open-file fname t lineno)))))))))

(defun org-logseq-new-page (page)
  "Create a new PAGE org file in pages directory if setting
 `org-logseq-new-page-p' to non-nil."
  (if org-logseq-new-page-p
      (find-file-other-window
       (expand-file-name (concat "pages/" page) github-dir))
    (user-error "No page found; Check `org-logseq-new-page-p` variable")))

;; todo: looking for a way to open draw file by post
(defun org-logseq-open-excalidraw nil)

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
  (let ((excalidraw-fname (format "%s.excalidraw"
                                  (or name (format-time-string "%Y-%m-%d_%H_%M_%S")))))
    (insert (format "[[draws/%s]]" excalidraw-fname))
    (with-current-buffer
        (find-file-noselect
         (expand-file-name (format "draws/%s" excalidraw-fname)
                           org-logseq-dir))
      (insert org-logseq-excalidraw)
      (save-buffer))))

;;; Contents sidebar
(defcustom org-logseq-sidebar-width 25
  "Sidebar window width")

(defcustom org-logseq-sidebar-side 'left
  "Sidebar position")

;;;###autoload
(defun org-logseq-contents-sidebar ()
  "Display contents.org as sidebar left side."
  (interactive)
  (display-buffer-in-side-window
   (find-file (expand-file-name "pages/contents.org" org-logseq-dir))
   (list (cons 'side org-logseq-sidebar-side)
         (cons 'window-width org-logseq-sidebar-width))))

;;; Logseq id overlays
(defvar-local org-logseq-block-ref-overlays nil)
(defvar-local org-logseq-buffer-modified-p nil)

(defvar org-logseq-block-ref-re "((\\([a-zA-Z0-9-]+\\)))")
(defvar org-logseq-block-embed-re "{{embed +((\\([a-zA-Z0-9-]+\\)))]}}")

(defface org-logseq-ref-block
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#fff3da" :extend t))
  "Face for transcluded block."
  :group 'org-logseq)

(defface org-logseq-embeded-block
  '((((class color) (min-colors 88) (background light))
     :background "#f3f3ff" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#f3f3ff" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

(defun org-logseq-toggle-block-ref-overlays ()
  (interactive)
  (setq org-logseq-buffer-modified-p (buffer-modified-p))
  (if org-logseq-block-ref-overlays
      (org-logseq-deactivate)
    (org-logseq-activate))
  (set-buffer-modified-p org-logseq-buffer-modified-p))

(defun org-logseq-activate ()
  (org-logseq-make-block-ref-overlays)
  (add-hook 'before-save-hook 'org-logseq-remove-block-ref-overlays nil t)
  (add-hook 'after-save-hook 'org-logseq-make-block-ref-overlays nil t))

(defun org-logseq-deactivate ()
  (org-logseq-remove-block-ref-overlays)
  (remove-hook 'before-save-hook 'org-logseq-remove-block-ref-overlays t)
  (remove-hook 'after-save-hook 'org-logseq-make-block-ref-overlays t))

(defun org-logseq-make-block-ref-overlays (&optional beg end)
  (save-excursion
    (goto-char (or beg (point-min)))
    (while (re-search-forward org-logseq-block-ref-re end t)
      (let ((overlay-end (point))
            (overlay-beg (re-search-backward "((" (line-beginning-position) t)))
        (org-logseq-create-block-ref-overlay overlay-beg overlay-end)))))

(defun org-logseq-create-block-ref-overlay (beg end)
  (let* ((tuuid (org-logseq-get-id beg end))
         (src-fh (org-logseq-get-block-ref-heading tuuid))
         (file (car src-fh))
         (heading (let ((result (cdr src-fh)))
                    (dolist (func org-logseq-block-ref-heading-hook) 
                      (setq result (funcall func result)))
                    result)))
    (delete-region beg end)
    (insert heading)
    (let* ((end (point))
           (ov (make-overlay beg end)))
      (overlay-put ov 'type 'block)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'file file)
      (overlay-put ov 'block-uuid (cdr tuuid))
      (overlay-put ov 'block-heading heading)
      (overlay-put ov 'help-echo
                   (format "Original page: %s.org" (file-name-base file)))
      (overlay-put ov 'face 'org-logseq-ref-block)
      (add-text-properties beg end '(read-only t))
      (push ov org-logseq-block-ref-overlays))))

(defun org-logseq-get-block-ref-heading (tuuid)
  (when-let ((result (shell-command-to-string
                      (org-logseq-grep-query tuuid))))
    (let* ((f-n (split-string result ":" nil))
           (file (car f-n))
           (line (string-to-number (cadr f-n))))
      (cons file
            (with-current-buffer (find-file-noselect file)
              (goto-line line)
              (org-no-properties (org-get-heading)))))))

(defun olih-link (heading)
  (if (string-match "\\[\\[.+\\]\\[\\(.+\\)\\]\\]" heading)
      (match-string 1 heading)
    heading))

(defcustom org-logseq-block-ref-heading-hook
  '(olih-link)
  "Hook for cleaning up id heading")

(defun org-logseq-remove-block-ref-overlays ()
  (when org-logseq-block-ref-overlays
    (save-excursion
      (dolist (ov org-logseq-block-ref-overlays)
        (let ((beg (overlay-start ov))
              (end (overlay-end ov))
              (block-uuid (overlay-get ov 'block-uuid))
              (file (overlay-get ov 'file))
              (inhibit-read-only t))
          (remove-text-properties beg end '(read-only t))
          (delete-region beg end)
          (delete-overlay ov)
          (goto-char beg)
          (insert (concat "((" block-uuid "))") ))))
    (setq org-logseq-block-ref-overlays nil)))

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
