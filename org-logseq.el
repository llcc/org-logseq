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

(defgroup org-logseq nil
  "Logseq capbility in org-mode")

(defcustom org-logseq-dir
  (expand-file-name "Org-Roam" github-dir)
  "logseq default path")

(defcustom org-logseq-create-page-p nil
  "Create a new page or not")

(defun org-logseq-get-page-or-id-at-point ()
  (save-excursion
    (when-let* ((prev-paren (search-backward-regexp
                             (regexp-opt '("[[" "((")) (line-beginning-position) t)))
      (let* ((next-paren (search-forward-regexp
                          (regexp-opt '("]]" "))")) (line-end-position) t))
             (page-or-id (buffer-substring-no-properties
                          (+ prev-paren 2) (- next-paren 2))))
        (cons (pcase (buffer-substring prev-paren (+ prev-paren 2))
                ("[[" 'page)
                ("((" 'id)) page-or-id)))))

(defun org-logseq-grep-query (id-or-page)
  (let ((type (car id-or-page))
        (search (cdr id-or-page)))
    (pcase type
      ('page (format "grep -niR \"^#+\\(TITLE\\): *%s\" \"%s\" --exclude-dir=\".git\"" search org-logseq-dir))
      ('id (format "grep -niR \"id: *%s\" \"%s\" --exclude-dir=\".git\"" search org-logseq-dir)))))

(defun org-logseq-create-page (page)
  (if org-logseq-create-page-p
      (find-file (expand-file-name (concat "pages/" page) github-dir))
    (user-error "No page found; Check `org-logseq-create-page` variable")))

(defun org-logseq-open-link ()
  (interactive)
  (let* ((tal (org-logseq-get-page-or-id-at-point))
         (results (shell-command-to-string (org-logseq-grep-query tal))))
    (if (string= results "")
        (org-logseq-create-page (cdr tal))
      (let* ((split (split-string results ":" nil))
             (filename (car split))
             (lineno (string-to-number (cadr split))))
        (org-open-file filename t lineno)))))

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
