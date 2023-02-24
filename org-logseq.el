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

(defgroup org-logseq nil
  "Logseq capbility in Org Mode."
  :group 'org)

(defcustom org-logseq-dir nil
  "Path of logseq notes."
  :group 'org-logseq)

(defcustom org-logseq-new-page-p nil
  "Non-nil means creating a page if not exist."
  :group 'org-logseq)

(defcustom org-logseq-block-ref-overlay-p nil
  "Non-nil means to enable block ref by default."
  :group 'org-logseq)
(make-variable-buffer-local 'org-logseq-block-ref-overlay-p)

(defcustom org-logseq-block-embed-overlay-p nil
  "Non-nil means to enable block ref by default."
  :group 'org-logseq)
(make-variable-buffer-local 'org-logseq-block-embed-overlay-p)


;; deprecate
(defun org-logseq-grep-query (page-or-id)
  "Return grep result for searching PAGE-OR-ID in `org-logseq-dir'."
  (let ((type (car page-or-id))
        (query (cdr page-or-id)))
    (format (pcase type
              ('page "rg -ni --no-heading -m 1 --type org -g '!.git' -g '!logseq' -g '!assets' '^#\\+(TITLE|ALIAS): *%s$' %s" )
              ('id "rg -ni --no-heading -m 1 --type org -g '!.git' -g '!logseq'  -g '!assets' ':id: *%s' %s"))
            query (shell-quote-argument org-logseq-dir))))

;; deprecate
(defun org-logseq-open-file-at-line-number (file-name line-number)
  "Open the given logseq file at LINE-NUMBER.
if the FILE-NAME is current buffer, jump to the line."
  (if (string-equal file-name (buffer-file-name))
      (progn
        (org-goto-line line-number)
        (when (equal (line-number-at-pos) line-number)
          (evil-open-fold)
          (org-goto-line line-number)))
    (org-open-file file-name t line-number))
  (evil-close-fold)
  (org-fold-show-children))


(defun is-uuid (s)
  "Return non-nil if S is a uuid, otherwise nil."
  ((lambda(s)(and(eq(string-bytes s)36)(let((l(string-to-list s))(i 0)(h '(8 13 18 23))(v t))(dolist(c l v)(set'v(and v(if(member i h)(and v(eq c 45))(or(and(> c 47)(< c 58))(and(> c 64)(< c 91))(and(> c 96)(< c 123))))))(set'i(+ i 1)))))) s))

(defun org-logseq-get-begin-value (key)
  "Get begin #+KEY:value of org."
  (cadar (org-collect-keywords (list key))))

(defun org-logseq-set-begin-value (key value)
  "Set the value of the first occurence of #+KEY: VALUE add it at the beginning of file if there is none."
  (let* ((key (concat "#+" key ": "))
        (new-key-value (concat key value)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
          (concat (regexp-quote key) "\.\*\$") nil t)
          (replace-match new-key-value nil nil)
        (insert (concat new-key-value "\n"))))))

(defun org-logseq-get-block-id ()
  "Return a cons: \"('id . id)\" at point."
  (save-excursion
    (when-let* ((prev-bracket (search-backward-regexp "((" (line-beginning-position) t)))
      (let* ((next-bracket (search-forward-regexp "))" (line-end-position) t))
             (id (buffer-substring-no-properties
                  (+ prev-bracket 2) (- next-bracket 2))))
        (cons 'id id)))))

(defun org-logseq-get-link ()
  "Return a cons: \"('type . link)\" at point. The type can be 'url, 'page, denoting the link type."
  (save-excursion
    (let ((context (org-element-context)) link)
      (when (eq 'link (car context))
        (setq link (org-element-property :raw-link context))
        (cond ((string-match "\\(?:https?\\)" link)
               (cons 'url link))
              (t (cons 'page link)))))))

(defun org-logseq-get-block-ref-from-overlay ()
  "Return \"('id . uuid)\" if point is a overlay created by org-logseq."
  (when-let ((ov (ov-at (point)))
             (create-by-org-logseq-flag (eq (ov-val ov 'parent) 'block)))
    (cons 'overlay (ov-val ov 'block-uuid))))

(defun org-logseq-get-file-name-from-title (title-name)
  "Return file name with path by the TITLE-NAME."
  (if (string-match (rx string-start (= 3 alpha) ", " (group (= 4 digit)) "/"
                        (group (= 2 digit)) "/" (group (= 2 digit)) string-end) title-name)
      (expand-file-name (concat "journals/"
                                (match-string 1 title-name) "_"
                                (match-string 2 title-name) "_"
                                (match-string 3 title-name) ".org")
                        org-logseq-dir)
    (expand-file-name (concat
                       "pages/" (string-replace "/" "___" title-name) ".org")
                      org-logseq-dir)))

(defun org-logseq-open-page-inside (title)
  "Open logseq by TITLE inside Emacs."
  (let (file-name)
    (if (string-match (rx string-start (= 3 alpha) ", " (group (= 4 digit)) "/"
                          (group (= 2 digit)) "/" (group (= 2 digit)) string-end) title)
        (setq file-name (concat "journals/" (match-string 1 title) "_"
                                (match-string 2 title) "_" (match-string 3 title)
                                ".org"))
      (setq file-name (concat "pages/" (string-replace "/" "___" title) ".org")))
    (setq file-name (expand-file-name file-name org-logseq-dir))
    (if (file-exists-p file-name)
        (org-open-file file-name t)
      (org-logseq-create-new-page title))
    ))

(defun org-logseq-update-id-locations ()
  "Update id locations in logseq directories."
  (org-id-update-id-locations
   (append
    (directory-files (expand-file-name "journals" org-logseq-dir) t ".*org")
    (directory-files (expand-file-name "pages" org-logseq-dir) t ".*org"))))

(defun org-logseq-find-id-file (id)
  "Find ID location by 'org-id-find-id-file'.
If can't find update the id locations and try again."
  (let ((file-name (file-truename (org-id-find-id-file id))))
    (if file-name
        file-name
      (progn
        (message "Not found id, update the id locations.")
        (org-logseq-update-id-locations)
        (setq file-name (file-truename (org-id-find-id-file id)))
        (if file-name
            file-name
          (user-error "Can not find id: \"%s\"" id))))))

(defun org-logseq-goto-id (id)
  "Goto ID."
  (let ((file-name (org-logseq-find-id-file id)))
    (if (not (string-equal (buffer-file-name) file-name))
        (find-file-other-window file-name))
    (org-id-goto id)
    (org-fold-hide-subtree)
    (org-show-children)))

(defun org-logseq-open-external (title-or-id)
  "Change logseq page through xdg-open by TITLE-OR-ID.
But not change the keyboard focus.
In order to use this function, you need to manually open logseq in advance."
  (shell-command
   (concat "xdg-open \"logseq://graph/Logseq_notes?" title-or-id "\""))
  (shell-command
   (concat "xdotool search --name \"" (shell-quote-argument (frame-parameter nil 'name))
           "\" windowactivate %1"))
  ;; (call-process-shell-command
  ;;  (concat "xdg-open \"logseq://graph/Logseq_notes?" title-or-id "\";" "xdotool search --name \"" (shell-quote-argument (frame-parameter nil 'name))
  ;;          "\" windowactivate %1"))
  )

(defun org-logseq-open-external-by-uuid (block-id)
  "Open logseq by BLOCK-ID."
  (org-logseq-open-external (concat "block-id=" block-id)))

(defun org-logseq-get-link-at-point ()
  "Return (type link) at current point."
  (or (org-logseq-get-block-ref-from-overlay)
      (org-logseq-get-link)
      (org-logseq-get-block-id)))

(defun org-logseq-create-new-page (title-name)
    "Create a new org file in pages directory according to TITLE-NAME."
    (let ((page-name
           (expand-file-name (concat
                              "pages/" (string-replace "/" "___" title-name) ".org")
                             org-logseq-dir)))
      (if (y-or-n-p (format "The file \"%s\" doesn't exist, create it or not?" page-name))
          (progn
            (find-file-other-window page-name)
            (org-logseq-set-begin-value "title" title-name))
        (message (format "The file \"%s\" doesn't exist." page-name)))))

;;;###autoload
(defun org-logseq-evil-close-fold ()
  "Set the heading's collapsed property to true after close fold."
  (interactive)
  (evil-close-fold)
  (org-set-property "collapsed" "true"))

;;;###autoload
(defun org-logseq-evil-open-fold ()
  "Set the heading's collapsed property to false after open fold."
  (interactive)
  (org-show-children)
  (org-set-property "collapsed" "false"))

;;;###autoload
(defun org-logseq-evil-close-folds ()
  "Close all folds by 'evil-close-folds', and set the first level's collapsed property to ture."
  (interactive)
  (evil-close-folds)
  (org-map-entries '(org-set-property "collapsed" "true") "LEVEL=1"))

;;;###autoload
(defun org-logseq-open-external-by-title (&optional title)
  "Open logseq page by current buffer's #+title or TITLE."
  (interactive)
  ;; TODO handel the situation that there is no page named title.
  (if (not (when-let ((title (or title (org-logseq-get-begin-value "title")))
                      (title-link (concat "page=" title)))
             (org-logseq-open-external title-link)))
      (message "There is not #+TITLE or #+title property in current buffer.")))

;;;###autoload
;; TODO intergate with org-logseq-open-link
(defun org-logseq-open-external-at-point ()
  "Open logseq page by block-id."
  (interactive)
  (message
   (catch 'exit
     (let (type-link type link)
       ;; https://codegolf.stackexchange.com/a/66501
       (save-window-excursion
         (while (or (setq type-link (org-logseq-get-link-at-point))
                    (org-up-heading-or-point-min))
           (if type-link
               (progn
                 (setq type (car type-link))
                 (setq link (cdr type-link))
                 (pcase type
                   ('url
                    (browse-url link)
                    (throw 'exit "Open url."))
                   ('page (org-logseq-open-external-by-title link)
                          (throw 'exit "Open page according to current block or it's parent's block."))
                   (_ (org-logseq-open-external-by-uuid link)
                      (throw 'exit "Open block-id at current block or it's parent's block")
                      ))))))
       (org-logseq-open-external-by-title)
       (throw 'exit "Open current page")))))

;;;###autoload
(defun org-logseq-update-selected-file-timestamp ()
  "Update logseq files last-update-time property."
  (interactive)
  (save-buffer)
  (save-excursion
    (set-buffer logseq-current-buffer)
    (org-logseq-set-begin-value "last-update-time" (current-time-string))
    (save-buffer)))

;;;###autoload
(defun org-logseq-select-current-buffer ()
  "Set logseq-current-bufer to current buffer."
  (interactive)
  (setq logseq-current-buffer (current-buffer))
  (message "logseq-current-buffer set to %s" (buffer-name)))

;;;###autoload
(defun org-logseq-set-title ()
  "Set the #+title property according to current logseq buffer filename."
  (interactive)
  (let ((title (org-get-title)) file-name)
    (if title
        (message "The #+title property already exists")
      (progn
        (setq file-name (file-name-base (buffer-file-name)))
        (if (string-match (rx string-start (group (= 4 digit)) "_" (group (= 2 digit))
                              "_" (group (= 2 digit)) string-end)
                          file-name)
            (progn
              (let* ((year (string-to-number (match-string 1 file-name)))
                     (month (string-to-number (match-string 2 file-name)))
                     (day (string-to-number (match-string 3 file-name)))
                     (day-of-week
                      (nth (org-day-of-week day month year)
                           '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))))
                (setq title (format "%s, %s/%s/%s" day-of-week year month day))
                (message (format "Current buffer is a journal file, set the title to %s" title))))
          (progn
            (setq title (string-replace "___" "/" file-name))
            (message (format "Current buffer is a page file, set the title to %s" title))))
        (org-logseq-set-begin-value "title" title)))))

;;;###autoload
(defun org-logseq-open-at-point-inside ()
  "Open link at point in Emacs or browser, supports url, id, page."
  (interactive)
  (when-let ((type-link (org-logseq-get-link-at-point)))
    (let ((type (car type-link))
          (link (cdr type-link)))
      (pcase type
        ('url (browse-url link))
        ('page (org-logseq-open-page-inside link))
        (_ (org-logseq-goto-id link))))))

;;;###autoload
(defun org-logseq-open-at-point-external ()
  "Open link at point in Logseq or browser, suports url, id, page."
  (interactive)
  (when-let ((type-link (org-logseq-get-link-at-point)))
    (let ((type (car type-link))
          (link (cdr type-link)))
      (pcase type
        ('url (browse-url link))
        ('page (org-logseq-open-external-by-title link))
        (_ (org-logseq-open-external-by-uuid link))))))

;;;###autoload

;;;###autoload

;;; Logseq id overlays

;;;###Variable for another functions.
;; (defvar org-logseq-block-ref-re "((\\([a-zA-Z0-9-]+\\)))")
(defvar org-logseq-block-ref-re
  (rx "((" (group (= 8 alnum) "-"
                  (= 3 (= 4 alnum) "-")
                  (= 12 alnum)) "))"))

(defun org-logseq-replace-todo (heading)
  "If HEADING start with TODO DO DONE, replace them."
  (while (string-match (rx word-start (| "TODO" "DOING" "DONE" "CANCELED") word-end " ") heading)
    (setq heading
          (concat (substring heading nil (match-beginning 0))
                  (substring heading (match-end 0) nil))))
  heading)

(defun org-logseq-replace-link (heading)
  "If HEADING contain [[link][title],  only show title."
  (while (string-match "\\[\\[.+\\]\\[\\(.+\\)\\]\\]" heading)
    (setq heading
          (concat (substring heading nil (match-beginning 0))
                  "🌐"
                  (match-string 1 heading)
                  (substring heading (match-end 0) nil))))
  heading)

;; (defcustom org-logseq-block-ref-heading-hook
;;   '(org-logseq-replace-link org-logseq-replace-block-ref org-logseq-replace-todo)
;;   "Hook for cleaning up id heading.")

(defun org-logseq-make-block-ref-overlays ()
  "Insert ovelays at ref."
  (interactive)
  ;; (ov-clear)
  (let (ov uuid begin end heading-text)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-logseq-block-ref-re nil t)
        (setq uuid (match-string 1))
        (setq begin (match-beginning 0))
        (setq end (match-end 0))
        (if (and (is-uuid uuid)
                 (not (ov-in begin end)))
            (progn
              (setq ov (ov-make  begin end))
              (setq heading-text (org-logseq-get-block-ref-content uuid))
              (ov-set ov
                      'category 'block-ref
                      'display heading-text
                      'prev-display heading-text
                      'block-uuid uuid
                      'evaporate t
                      ;; 'target heading-text
                      'face 'org-inline-src-block
                      'front-sticky t
                      'rear-sticky t
                      'cursor-sensor-functions (list #'org-logseq-ov-cursor-sensor))
              ;; (ov-placeholder ov)
              ))))))

(defun org-logseq-ov-cursor-sensor (window position state)
  "Change the display of overlay at POSITION in WINDOW accordint to the STATE."
  (message "new-in")
  (setq disable-point-adjustment t)
  (let (( ov (ov-at
              (pcase state
                ('entered nil)
                ('left position)
                )
              )))
    (if (and ov (eq (ov-val ov 'category) 'block-ref))
        (pcase state
          ('entered
           (ov-set ov 'display nil)
           (goto-char (- (ov-end ov) 3))
           )
          ('left
           (save-excursion (ov-set ov 'display (org-logseq-get-block-ref-content (ov-val ov 'block-uuid))))
           )))))

(defun org-logseq-get-block-ref-content (uuid)
  "Return the content of the given UUID."
  (when-let ((file-name (org-logseq-find-id-file uuid)))
    (save-window-excursion
      (with-current-buffer (find-file-noselect file-name)
        (org-id-goto uuid)
        (let ((result (org-no-properties (org-get-heading))))
          (dolist (func  '(org-logseq-replace-link
                           org-logseq-replace-block-ref
                           org-logseq-replace-todo))
            (setq result (funcall func result)))
          result)))))

(defun org-logseq-replace-block-ref (heading)
  "If HEADING contain ((uuid)), replace it with the heading text."
    (while (string-match org-logseq-block-ref-re heading)
      (let ((uuid (match-string 1 heading))
            (end (match-end 0)))
        (if (is-uuid uuid)
          (setq heading
                (concat (substring heading nil (match-beginning 0))
                        (org-logseq-get-block-ref-content uuid)
                        (substring heading end nil)
                        )))))
  heading)


;; (defvar org-logseq-overlay-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "RET" 'org-logseq-open-link)
;;     map))

(defun org-logseq-activate ()
  "Override the default open behavior of org."
  (advice-add 'org-open-at-point :override #'org-logseq-open-at-point-inside)
  (advice-add 'org-open-at-mouse :override #'org-logseq-open-at-point-inside))

(defun org-logseq-deactivate ()
  "Restore the default open behavior of org."
  (advice-remove 'org-open-at-point #'org-logseq-open-at-point-inside)
  (advice-remove 'org-open-at-mouse #'org-logseq-open-at-point-inside))

(defvar org-logseq-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-open-at-point] 'org-logseq-open-at-point-inside)
    (define-key map [remap org-open-at-mouse] 'org-logseq-open-at-point-inside)
    map)
  "'org-logseq-mode' map.")

(define-minor-mode org-logseq-mode
  "Org-logseq minor mode."
  :init-value nil
  :global nil
  :keymap org-logseq-mode-map
  (cursor-sensor-mode t)
  )

;; (add-hook 'org-logseq-mode-hook
;;             #'(lambda ()
;;                 (add-hook 'after-save-hook 'org-logseq-make-block-ref-overlays nil 'make-it-local)))
;; (evil-define-key 'normal org-logseq-mode-map (kbd "zc") 'org-logseq-evil-close-fold)
;; (evil-define-key 'normal org-logseq-mode-map (kbd "zo") 'org-logseq-evil-open-fold)
(provide 'org-logseq)
;;; org-logseq.el ends here
