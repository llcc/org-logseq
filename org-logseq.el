;;; org-logseq.el --- for logseq  -*- lexical-binding: t; -*-

;; Author: Zhe Lei <lzhes43@gmail.com>
;; URL: https://github.com/llcc/org-logseq
;; Package-Version: 20210223.840
;; Version: 0.0.1

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
  "Logseq capbility in Org Mode")

(defcustom org-logseq-dir nil
  "logseq default path")

(defcustom org-logseq-create-page-p nil
  "Create a new page or not")

(defun org-logseq-get-id-at-point ()
  (save-excursion
    (when-let* ((prev-bracket (search-backward-regexp
                               "((" (line-beginning-position) t)))
      (let* ((next-bracket (search-forward-regexp
                            "))" (line-end-position) t))
             (page-or-id (buffer-substring-no-properties
                          (+ prev-bracket 2) (- next-bracket 2))))
        (cons 'id page-or-id)))))

(defun org-logseq-get-path-at-point ()
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
  (let ((type (car page-or-id))
        (query (cdr page-or-id)))
    (format (pcase type
              ('page "grep -niR \"^#+\\(TITLE\\): *%s\" \"%s\" --exclude-dir=\".git\"" )
              ('id "grep -niR \"^:id: *%s\" \"%s\" --exclude-dir=\".git\""))
            query org-logseq-dir)))

(defun org-logseq-create-page (page)
  (if org-logseq-create-page-p
      (find-file (expand-file-name (concat "pages/" page) github-dir))
    (user-error "No page found; Check `org-logseq-create-page` variable")))

;;;###autoload
(defun org-logseq-open-link ()
  (interactive)
  (when-let* ((t-l (or (org-logseq-get-path-at-point)
                       (org-logseq-get-id-at-point))))
    (pcase (car t-l)
      ('url (browse-url (cdr t-l)))
      ('draw (org-logseq-open-draw (cdr t-l)))
      (t (let ((result (shell-command-to-string (org-logseq-grep-query t-l))))
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
}")

(defun org-logseq-new-excalidraw (&optional name)
  (interactive "P")
  (let ((draw-fname (format "%s.excalidraw"
                            (or name (format-time-string "%Y-%m-%d_%H_%M_%S")))))
    (insert (format "[[draws/%s]]" draw-fname))
    (with-current-buffer
        (find-file-noselect (expand-file-name (format "draws/%s" draw-fname)
                                              org-logseq-dir))
      (insert org-logseq-excalidraw)
      (save-buffer))))

(define-minor-mode org-logseq-mode
  "Org-logseq minor mode"
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap org-open-at-point] 'org-logseq-open-link)
            map))

(provide 'org-logseq)

;;; org-logseq.el ends here
