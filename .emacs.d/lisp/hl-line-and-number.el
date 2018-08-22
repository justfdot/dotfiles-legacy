(require 'hl-line)

(defface justf/linum-hl
  `((t :inherit linum :background "#282a2e" :foreground "#f0c674"))
  "Face for the current line number."
  :group 'linum)

(defvar justf/linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'justf/linum-get-format-string)

(defun justf/linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                            (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d ")))
    (setq justf/linum-format-string format)))

(defvar justf/linum-current-line-number 0)

(setq linum-format 'justf/linum-format)

(defun justf/linum-format (line-number)
  (propertize (format justf/linum-format-string line-number) 'face
              (if (eq line-number justf/linum-current-line-number)
                  'justf/linum-hl
                  'linum)))

(defadvice linum-update (around justf/linum-update)
  (let ((justf/linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(provide 'hl-line-and-number)
