;;; org-logseq.el --- for logseq  -*- lexical-binding: t; -*-

;; Author: Zhe Lei <lzhes43@gmail.com>
;; URL: https://github.com/llcc/org-logseq
;; Package-Version: 20210402.2237
;; Version: 0.0.4
;; Package-Requires: ((dash "2.11.0") (org "9.0.0"))


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
(require 'org-element)
(require 'dash)

(defgroup org-logseq nil
  "Logseq capbility in Org Mode"
  :group 'org)

(defcustom org-logseq-dir nil
  "Path of logseq notes"
  :group 'org-logseq)

(defcustom org-logseq-new-page-p nil
  "Non-nil means creating a page if not exist."
  :group 'org-logseq)

(defcustom org-logseq-block-ref-overlay-p nil
  "Non-nil means to enable block ref by default"
  :group 'org-logseq)
(make-variable-buffer-local 'org-logseq-block-ref-overlay-p)

(defcustom org-logseq-block-embed-overlay-p nil
  "Non-nil means to enable block ref by default"
  :group 'org-logseq)
(make-variable-buffer-local 'org-logseq-block-embed-overlay-p)

(defun org-logseq-get-block-id (&optional beg end embed)
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
              ('page "grep -niR \"^#+\\(TITLE\\|ALIAS\\): *%s\" %s --exclude-dir=.git" )
              ('id "grep -niR \":id: *%s\" %s --exclude-dir=.git"))
            query (shell-quote-argument (f-expand org-logseq-dir)))))

(defun org-logseq-get-block-ref-or-embed-link ()
  (when-let ((ovs (overlays-at (point)))
             (ov (--first (eq (overlay-get it 'parent) 'block) ovs)))
    (cons 'id (overlay-get ov 'block-uuid))))

;;;###autoload
(defun org-logseq-open-link ()
  "Open link at point. Supports url, id and page.
or Block Ref or Embed overlays."
  (interactive)
  (when-let* ((t-l (or (org-logseq-get-block-ref-or-embed-link)
                       (org-logseq-get-link)
                       (org-logseq-get-block-id))))
    (let ((type (car t-l))
          (link (cdr t-l)))
      (pcase type
        ('url (browse-url link))
        ('draw (org-logseq-open-excalidraw link))
        (_ (let ((result (shell-command-to-string
                          (org-logseq-grep-query t-l))))
             (when (string-prefix-p "grep" result)
               (error "grep searching error"))
             (if (string= result "")
                 (org-logseq-new-page link)
               (let* ((f-n (split-string result ":" nil))
                      (fname (car f-n))
                      (lineno (string-to-number (cadr f-n))))
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
(defcustom org-logseq-sidebar-width 30
  "Sidebar window width")

(defcustom org-logseq-sidebar-side 'left
  "Sidebar position")

;;;###autoload
(defun org-logseq-toggle-contents-sidebar ()
  "Display contents.org as sidebar left side."
  (interactive)
  (if-let ((contents-window
            (--first
             (and (string= "contents.org" (buffer-name (window-buffer it)))
                  (window-dedicated-p it))
             (window-list (selected-frame)))))
      (delete-window contents-window)
    (let ((window-parameters (list (cons 'no-other-window t)
                                   (cons 'no-delete-other-windows t))))
      (display-buffer-in-side-window
       (find-file-noselect (expand-file-name "pages/contents.org" org-logseq-dir))
       (list (cons 'side org-logseq-sidebar-side)
             (cons 'window-width org-logseq-sidebar-width)
             (cons 'window-parameters window-parameters))))))

;;; Logseq id overlays
(defvar-local org-logseq-block-ref-overlays nil)
(defvar-local org-logseq-block-embed-overlays nil)
(defvar-local org-logseq-buffer-modified-p nil)

(defvar org-logseq-block-ref-re "((\\([a-zA-Z0-9-]+\\)))")
(defvar org-logseq-block-embed-re "{{embed +((\\([a-zA-Z0-9-]+\\))) *}}")

(defface org-logseq-ref-block
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#fff3da" :extend t))
  "Face for ref block."
  :group 'org-logseq)

(defface org-logseq-embed-block
  '((((class color) (min-colors 88) (background light))
     :background "#f3f3ff" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#f3f3ff" :extend t))
  "Face for embed block."
  :group 'org-logseq)

;;;###autoload
(defun org-logseq-toggle-block-ref-overlays ()
  (interactive)
  (if org-logseq-block-ref-overlays
      (org-logseq-block-ref-deactivate)
    (org-logseq-block-ref-activate)))

;;;###autoload
(defun org-logseq-toggle-block-embed-overlays ()
  (interactive)
  (if org-logseq-block-embed-overlays
      (org-logseq-block-embed-deactivate)
    (org-logseq-block-embed-activate)))

(defun org-logseq--make-block-ref-overlays ()
  (org-logseq-make-block-overlays 'ref))
(defun org-logseq--remove-block-ref-overlays ()
  (org-logseq-remove-block-overlays 'ref))

(defun org-logseq-block-ref-activate ()
  (org-logseq--make-block-ref-overlays)
  (add-hook 'before-save-hook #'org-logseq--remove-block-ref-overlays nil t)
  (add-hook 'after-save-hook #'org-logseq--make-block-ref-overlays nil t)
  (add-hook 'kill-buffer-hook #'org-logseq--remove-block-ref-overlays nil t))
(defun org-logseq-block-ref-deactivate ()
  (org-logseq--remove-block-ref-overlays)
  (remove-hook 'before-save-hook #'org-logseq--remove-block-ref-overlays t)
  (remove-hook 'after-save-hook #'org-logseq--make-block-ref-overlays t)
  (remove-hook 'kill-buffer-hook #'org-logseq--make-block-ref-overlays t))

(add-hook 'org-mode-hook
          #'(lambda () (when org-logseq-block-ref-overlay-p
                         (org-logseq-block-ref-activate))))

(defun org-logseq--make-block-embed-overlays ()
  (org-logseq-make-block-overlays 'embed))
(defun org-logseq--remove-block-embed-overlays ()
  (org-logseq-remove-block-overlays 'embed))

(defun org-logseq-block-embed-activate ()
  (org-logseq--make-block-embed-overlays)
  (add-hook 'before-save-hook #'org-logseq--remove-block-embed-overlays nil t)
  (add-hook 'after-save-hook #'org-logseq--make-block-embed-overlays nil t))
(defun org-logseq-block-embed-deactivate ()
  (org-logseq--remove-block-embed-overlays)
  (remove-hook 'before-save-hook #'org-logseq--remove-block-embed-overlays t)
  (remove-hook 'after-save-hook #'org-logseq--make-block-embed-overlays t))

(add-hook 'org-mode-hook
          #'(lambda () (when org-logseq-block-embed-overlay-p
                         (org-logseq-block-embed-activate))))

(defun org-logseq-make-block-overlays (type &optional beg end)
  (setq org-logseq-buffer-modified-p (buffer-modified-p))
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((re (pcase type
                ('ref org-logseq-block-ref-re)
                ('embed org-logseq-block-embed-re))))
      (while (and (re-search-forward re end t)
                  (pcase type
                    ('ref (not (looking-at " *}}")))
                    ('embed t)))
        (let* ((uuid (match-string-no-properties 1))
               (tuuid (cons 'id uuid))
               (overlay-end (point))
               (overlay-beg (re-search-backward
                             (pcase type ('ref "((") ('embed "{{"))
                             (line-beginning-position) t))
               (file-type-block (org-logseq-get-block-content tuuid type)))
          (org-logseq-create-block-overlay overlay-beg overlay-end file-type-block)))))
  (set-buffer-modified-p org-logseq-buffer-modified-p))

(defun org-logseq-prepare-embed-content (content)
  (let ((content-list (split-string content "\n"))
        headline-spaces line-spaces)
    (concat "​" (mapconcat
                #'(lambda (str)
                    (if (string-match "\\(\*+\\)" str)
                        (progn
                          (setq line-spaces (make-string (* (length (match-string 1 str)) 2) ? ))
                          (setq headline-spaces (make-string (* (1- (length (match-string 1 str))) 2) ? ))
                          (setq heading
                                (concat headline-spaces (replace-match "\*" nil nil str 1))))
                      (concat line-spaces str)))
                content-list "\n")
            "\n")))

(defun org-logseq-hide-block-embed-drawer-all (beg end)
  "Fold all drawers in the block embed overlays."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward org-drawer-regexp end t)
      (let* ((pair (get-char-property-and-overlay (line-beginning-position)
						                          'invisible))
	         (o (cdr-safe pair)))
	    (if (overlayp o) (goto-char (overlay-end o)) ;invisible drawer
	      (pcase (get-char-property-and-overlay (point) 'invisible)
	        (`(outline . ,o) (goto-char (overlay-end o))) ;already folded
	        (_
	         (let* ((drawer (org-element-at-point))
		            (type (org-element-type drawer)))
	           (when (memq type '(drawer property-drawer))
		         (org-hide-drawer-toggle t nil drawer)
		         ;; Make sure to skip drawer entirely or we might flag it
		         ;; another time when matching its ending line with
		         ;; `org-drawer-regexp'.
		         (goto-char end))))))))))

(defun org-logseq-create-block-overlay (beg end file-type-block)
  (pcase-let ((`((,file . ,uuid) . (,type . ,content))
               file-type-block))
    (delete-region beg end)
    (insert (pcase type
              ('ref content)
              ('embed (org-logseq-prepare-embed-content content))))
    (let* ((end (point))
           (ov (make-overlay beg end))
           (face (pcase type
                   ('ref 'org-logseq-ref-block)
                   ('embed 'org-logseq-embed-block))))
      (overlay-put ov 'parent 'block)
      (overlay-put ov 'type type)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'file file)
      (overlay-put ov 'block-uuid uuid)
      (overlay-put ov 'target content)
      (overlay-put ov 'help-echo
                   (format "Original page: %s.org" (file-name-base file)))
      (overlay-put ov 'face face)
      ;; (overlay-put ov 'keymap org-logseq-overlay-map)
      (add-text-properties beg end '(read-only t))
      (org-logseq-hide-block-embed-drawer-all beg end)
      (pcase type
        ('ref (push ov org-logseq-block-ref-overlays))
        ('embed (push ov org-logseq-block-embed-overlays))))))

(defvar org-logseq-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" 'org-logseq-open-link)
    map))

(defun org-logseq-get-block-content (tuuid type)
  (when-let ((to-marker (point-marker))
             (result (shell-command-to-string
                      (org-logseq-grep-query tuuid))))
    (let* ((f-n (split-string result ":" nil))
           (file (car f-n))
           (line (string-to-number (cadr f-n))))
      (cons (cons file (cdr tuuid))
            (with-current-buffer (find-file-noselect file)
              (goto-line line)
              (cons type
                    (pcase type
                      ('ref (format "[[id:%s][%s]]" (cdr tuuid)
                                    (let ((result (org-no-properties (org-get-heading))))
                                      (dolist (func org-logseq-block-ref-heading-hook) 
                                        (setq result (funcall func result)))
                                      result)))
                      ('embed 
                       (save-restriction
                         (org-narrow-to-subtree)
                         (buffer-substring-no-properties (point-min) (point-max)))))))))))

(defun olih-link (heading)
  (if (string-match "\\[\\[.+\\]\\[\\(.+\\)\\]\\]" heading)
      (match-string 1 heading)
    heading))

(defcustom org-logseq-block-ref-heading-hook
  '(olih-link)
  "Hook for cleaning up id heading")

(defun org-logseq-remove-block-overlays (type)
  (setq org-logseq-buffer-modified-p (buffer-modified-p))
  (when-let ((overlays (pcase type
                         ('ref org-logseq-block-ref-overlays)
                         ('embed org-logseq-block-embed-overlays))))
    (save-excursion
      (dolist (ov overlays)
        (let* ((beg (overlay-start ov))
               (end (overlay-end ov))
               (block-uuid (overlay-get ov 'block-uuid))
               (file (overlay-get ov 'file))
               (inhibit-read-only t))
          (remove-text-properties beg end '(read-only t))
          (delete-region beg end)
          (delete-overlay ov)
          (goto-char beg)
          (insert (pcase type
                    ('ref (concat "((" block-uuid "))"))
                    ('embed (format "{{embed  ((%s))}}" block-uuid))))))
      (pcase type
        ('ref (setq org-logseq-block-ref-overlays nil))
        ('embed (setq org-logseq-block-embed-overlays nil)))))
  (set-buffer-modified-p org-logseq-buffer-modified-p))

(defun org-logseq-return (&optional indent arg interactive)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' with ARG, otherwise call `newline' with ARG
and INTERACTIVE.

When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive "i\nP\np")
  (let ((context (if org-return-follows-link (org-element-context)
		           (org-element-at-point))))
    (cond
     ;; In a table, call `org-table-next-row'.  However, before first
     ;; column or after last one, split the table.
     ((or (and (eq 'table (org-element-type context))
	           (not (eq 'table.el (org-element-property :type context)))
	           (>= (point) (org-element-property :contents-begin context))
	           (< (point) (org-element-property :contents-end context)))
	      (org-element-lineage context '(table-row table-cell) t))
      (if (or (looking-at-p "[ \t]*$")
	          (save-excursion (skip-chars-backward " \t") (bolp)))
	      (insert "\n")
	    (org-table-justify-field-maybe)
	    (call-interactively #'org-table-next-row)))
     ;; On a link or a timestamp, call `org-open-at-point' if
     ;; `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
           (or (and (eq 'link (org-element-type context))
		            ;; Ensure point is not on the white spaces after
		            ;; the link.
		            (let ((origin (point)))
		              (org-with-point-at (org-element-property :end context)
			            (skip-chars-backward " \t")
			            (> (point) origin))))
	           (org-in-regexp org-ts-regexp-both nil t)
	           (org-in-regexp org-tsr-regexp-both nil  t)
	           (org-in-regexp org-link-any-re nil t)
               (org-logseq-get-block-id)
               (org-logseq-get-block-ref-or-embed-link)))
      (org-logseq-open-link))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
	       (let ((case-fold-search nil))
	         (org-match-line org-complex-heading-regexp)))
      ;; At headline.  Split line.  However, if point is on keyword,
      ;; priority cookie or tags, do not break any of them: add
      ;; a newline after the headline instead.
      (let ((tags-column (and (match-beginning 5)
			                  (save-excursion (goto-char (match-beginning 5))
					                          (current-column))))
	        (string
	         (when (and (match-end 4) (org-point-in-group (point) 4))
	           (delete-and-extract-region (point) (match-end 4)))))
	    ;; Adjust tag alignment.
	    (cond
	     ((not (and tags-column string)))
	     (org-auto-align-tags (org-align-tags))
	     (t (org--align-tags-here tags-column))) ;preserve tags column
	    (end-of-line)
	    (org-show-entry)
	    (org--newline indent arg interactive)
	    (when string (save-excursion (insert (org-trim string))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and (not (eolp))
	       (org-element-lineage context '(item)))
      (let ((trailing-data
	         (delete-and-extract-region (point) (line-end-position))))
	    (org--newline indent arg interactive)
	    (save-excursion (insert trailing-data))))
     (t
      ;; Do not auto-fill when point is in an Org property drawer.
      (let ((auto-fill-function (and (not (org-at-property-p))
				                     auto-fill-function)))
	    (org--newline indent arg interactive))))))

(defun org-logseq-activate ()
  (advice-add 'org-return :override #'org-logseq-return)
  (advice-add 'org-open-at-point :override #'org-logseq-open-link)
  (advice-add 'org-open-at-mouse :override #'org-logseq-open-link))

(defun org-logseq-deactivate ()
  (advice-remove 'org-return #'org-logseq-return)
  (advice-remove 'org-open-at-point #'org-logseq-open-link)
  (advice-remove 'org-open-at-mouse #'org-logseq-open-link))

(defvar org-logseq-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-return] 'org-logseq-return)
    (define-key map [remap org-open-at-point] 'org-logseq-open-link)
    (define-key map [remap org-open-at-mouse] 'org-logseq-open-link)
    map)
  "Org-logseq map")

(define-minor-mode org-logseq-mode
  "Org-logseq minor mode"
  :init-value nil
  :global nil
  :keymap org-logseq-map)

;; (if org-logseq-mode
;;     (org-logseq-activate)
;;   (org-logseq-deactivate))

;;;###autoload
(defun org-logseq-download-images ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "https://cdn.logseq.com" nil t)
      (let* ((context (org-element-context)))
        (when (eq (car context) 'link)
          (let ((link-begin (org-element-property :begin context))
                (link-end (org-element-property :end context))
                (path (org-element-property :raw-link context))
                (image-path (concat "images/"
                                    (make-temp-name "org-logseq-") (format-time-string "-%Y%m%d.png"))))
            (start-process "org logseq" nil "curl" path "--output" image-path)
            (setf (buffer-substring link-begin link-end) (format "[[./%s]]" image-path))))))))

(provide 'org-logseq)
;;; org-logseq.el ends here
