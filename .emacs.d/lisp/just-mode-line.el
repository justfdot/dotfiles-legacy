(column-number-mode 1)
(size-indication-mode 1)

(defface mode-line-dark
  '((t :foreground "#757876"))
  "Face used for non-prior parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-light
  '((t :foreground "#c5c8c6"))
  "Face used for prior parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

;; (defface mode-line-directory
;;   ;; '((t :foreground "#c5c8c6"))
;;   '((t :foreground "#757876"))
;;   "Face used for buffer identification parts of the mode line."
;;   :group 'mode-line-faces
;;   :group 'basic-faces)

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat "…/" output)))
       output))

(defvar mode-line-buffer-modified
  '(:eval (let ((marker (if buffer-read-only (if (buffer-modified-p) "" "") ""))
                (color (if buffer-read-only "#8abeb7" (if (buffer-modified-p) "#f0c674" "#282a2e"))))
            (propertize marker 'face `(:foreground ,color :height 75 :family "FontAwesome"))))
  "Formats the buffer modified or read-only mark.")
(put 'mode-line-buffer-modified 'risky-local-variable t)

(defvar mode-line-directory
  '(:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 15)) " "))
  ;; '(:propertize
  ;;   (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
  ;;               face mode-line-dark)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(defvar mode-line-buffer-size
  '(:eval (let* ((buffer-size " %I")
                 (code (symbol-name buffer-file-coding-system))
                 (eol-type (coding-system-eol-type buffer-file-coding-system))
                 (eol (pcase eol-type
                        (`0 "UNIX") (`1 "DOS") (`2 "MAC") (_ "???"))))
            (propertize buffer-size
                        ;; 'face 'mode-line-dark
                        'help-echo (concat "Encoding: " code "\nEOL: " eol))))
                 ;; (eol (if (eq 0 eol-type) "UNIX"
                 ;;        (if (eq 1 eol-type) "DOS"
                 ;;          (if (eq 2 eol-type) "MAC"
                 ;;            "???")))))
  "Formats the buffer size and make tooltip of it encoding.")
(put 'mode-line-buffer-size 'risky-local-variable t)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(defvar mode-line-buffer-modes
  (list "Modes: "
        `(:propertize ("" mode-name)
                      face mode-line-light
                      help-echo "Major mode"
                      mouse-face mode-line-highlight
                      local-map ,mode-line-major-mode-keymap)
        '("" mode-line-process)
        `(:propertize ("" minor-mode-alist)
                      help-echo "Minor mode"
                      mouse-face mode-line-highlight
                      local-map ,mode-line-minor-mode-keymap))
  "Mode line construct for displaying major and minor modes.")
(put 'mode-line-buffer-modes 'risky-local-variable t)

(setq-default mode-line-format
  '("%e"
    mode-line-front-space
    mode-line-buffer-modified
    mode-line-buffer-size
    mode-line-directory
    mode-line-buffer-identification
    ;; (:eval (propertize " on line " 'face 'mode-line-dark))
    " on line "
    (:eval (propertize "%l" 'face 'mode-line-light))
    ;; (:eval (propertize ", column " 'face 'mode-line-dark))
    ", column "
    (:eval (propertize "%c" 'face 'mode-line-light))
    "      " (flycheck-mode flycheck-mode-line) "      "
    mode-line-buffer-modes
    mode-line-misc-info
    mode-line-end-spaces))

    ;; mode-line-client
    ;; mode-line-mule-info
    ;; mode-line-remote -- no need to indicate this specially
    ;; mode-line-frame-identification -- this is for text-mode emacs only
    ;;(vc-mode vc-mode)  -- I use magit, not vc-mode

(provide 'just-mode-line)
