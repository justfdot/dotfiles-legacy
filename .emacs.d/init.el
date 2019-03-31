(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)
;; Loading some user scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)  ;; if you use any :bind variant

;; Regular font
;; (set-frame-font "Meslo LG S:pixelsize=14")
;; (set-frame-font "Office Code Pro:pixelsize=14")
(set-frame-font "Hack:pixelsize=14")
;; And for Daemon mode too
;; (add-to-list 'default-frame-alist (cons 'font "Meslo LG S:pixelsize=14"))
;; (add-to-list 'default-frame-alist (cons 'font "Office Code Pro:pixelsize=14"))
(add-to-list 'default-frame-alist (cons 'font "Hack:pixelsize=14"))

;; Store all backup and autosave files in one place
(setq backup-by-copying t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/auto/" t)))

;; Hide all these bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Automatic parens pairing and newlines around some chars
;; TODO: Look at smartparens package again
(electric-pair-mode 1)
(electric-layout-mode 1)

;; Highlight current line
;; (global-hl-line-mode 1)
;; Show native line numbers
;; (global-display-line-numbers-mode 1)

;; Make sure that UTF-8 is used everywhere.
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

;; Play around some hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; FIXME: enable highlighting in text modes too
(add-hook 'prog-mode-hook
                (lambda ()
                  (font-lock-add-keywords nil
                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-type-face t)))))


;; Store mini-buffer history
(use-package savehist
  :config
    (setq savehist-additional-variables
          ;; search entries
          '(search-ring regexp-search-ring)
          ;; keep the home clean
          savehist-file "~/.emacs.d/savehist")
    (savehist-mode +1))

;; Improve the mode-line
(require 'just-mode-line)

;; Toggle between vertical and horizontal split
(require 'toggle-window-split)

(use-package base16-theme
  :config (load-theme 'base16-tomorrow-night t))

(use-package eyebrowse
  :config
    (eyebrowse-mode t)
    (eyebrowse-setup-opinionated-keys))

(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      inhibit-startup-screen t)

(use-package evil
  :diminish magit-auto-revert-mode
            eldoc-mode
            evil-commentary-mode
            evil-local-mode-major-mode
            undo-tree-mode
  :bind (
    :map evil-normal-state-map
    ("C-h" . evil-window-left)
    ("C-j" . evil-window-down)
    ("C-k" . evil-window-up)
    ("C-l" . evil-window-right)
    ;; ("C-a" . evil-numbers/inc-at-pt)
    ;; ("C-x" . evil-numbers/dec-at-pt)
    ("C-<return>" . justf/evil-normal-newline-above)
    ("S-<return>" . justf/evil-normal-newline-above)
    ("RET" . justf/evil-normal-newline-below)
    ("j"   . evil-next-visual-line)
    ("k"   . evil-previous-visual-line)
    ("C-f" . ace-jump-char-mode)
    ("C-/" . swiper)
    ("C-?" . ivy-resume)
    ("*"   . justf/evil-search-word)
    :map evil-motion-state-map
    (":"   . evil-repeat-find-char)
    ("C-;" . evil-repeat-find-char)
    (";"   . evil-ex)
    ("C-f" . ace-jump-char-mode)
    :map Buffer-menu-mode-map
    ("RET" . justf/buffer-menu-select-and-kill-itself))

  :init

    (setq evil-want-C-u-scroll t
          evil-want-C-i-jump t
          ;; evil-want-fine-undo t
          evil-want-integration t
          ;; evil-want-minibuffer t
          evil-want-keybinding nil   ; required by evil-collection
          evil-split-window-below t
          evil-move-cursor-back nil
          evil-insert-state-cursor   '((bar . 2) "#f0c674")
          evil-normal-state-cursor   '(box "#c5c8c6")
          evil-visual-state-cursor   '(box "#b294bb")
          evil-operator-state-cursor '(box "#cc6666")
          evil-motion-state-cursor   '(box "#cc6666")
          evil-replace-state-cursor  '(box "#b5bd68"))

    (defun justf/evil-short-shift-width ()
      (setq-local evil-shift-width 2))
    (add-hook 'lisp-mode-hook 'justf/evil-short-shift-width)
    (add-hook 'emacs-lisp-mode-hook 'justf/evil-short-shift-width)
    ;; (add-hook 'web-mode-hook 'justf/evil-short-shift-width)

    (defun justf/evil-normal-newline-above ()
      "Add an empty line above the current line in normal state"
      (interactive)
      (move-beginning-of-line nil)
      (newline)
      (forward-line -1))

    (defun justf/evil-normal-newline-below ()
      "Add an empty line below the current line in normal state"
      (interactive)
      (end-of-line)
      (newline))

    (defun justf/evil-search-word ()
      "Search a word by pressing * but don't jump to the next match"
      (interactive)
      (with-no-warnings
        (evil-search-word-forward)
        (evil-search-previous)))

    (defun justf/buffer-menu-select-and-kill-itself ()
      "Kill *Buffer List* to avoid switch back to it by C-6"
      (interactive)
      (Buffer-menu-this-window)
      (kill-buffer (get-buffer "*Buffer List*")))

    (use-package evil-leader
      :init
        (setq evil-leader/leader "<SPC>")
        (global-evil-leader-mode)
      :config
        (evil-leader/set-key
          "<SPC>" 'counsel-M-x
          "-"  'evil-numbers/dec-at-pt
          "="  'evil-numbers/inc-at-pt
          "a"  'py-autopep8-buffer
          "b"  'ivy-switch-buffer
          "d"  'deer
          "e"  'flycheck-list-errors
          "f"  'counsel-projectile
          "k"  'kill-this-buffer
          "p"  'counsel-yank-pop
          "q"  'evil-save-and-close
          "r"  'counsel-recentf
          "v"  'exchange-point-and-mark
          "w"  'save-buffer
          "z"  'resize-window
          "hf" 'counsel-describe-function
          "hv" 'counsel-describe-variable
          "hk" 'describe-key))

    (evil-mode)
  :config

    ;; add evil movement keys to buffer-menu-mode
    (with-no-warnings
      (evil-set-initial-state 'Buffer-menu-mode 'normal)
      (evil-add-hjkl-bindings Buffer-menu-mode-map 'normal))

    (use-package evil-anzu
      :diminish anzu-mode
      :init
        (defun justf/anzu-mode-line-update-function (here total)
          (when anzu--state
                (let ((status (cl-case anzu--state
                              (search (format " Match %d of %d" here total))
                              (replace-query (format " %d replaces" total))
                              (replace (format " Replace %d of %d" here total)))))
                (propertize status 'face 'anzu-mode-line))))
        (global-anzu-mode t)
    ) ;; use-package evil-anzu ends here

    (use-package evil-surround
      :init (global-evil-surround-mode t))

    (use-package evil-org
      :after org
      :config
        (add-hook 'org-mode-hook 'evil-org-mode)
        (with-no-warnings
          (add-hook 'evil-org-mode-hook
                    (lambda () (evil-org-set-key-theme '(additional todo heading)))))
        (use-package org-bullets
          :after evil-org
          :init
            (setq org-ellipsis "…"
                  org-bullets-bullet-list '(" "))
          :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
    ) ;; use-package evil-org ends here

    (use-package evil-escape
      :diminish evil-escape-mode
      :init (evil-escape-mode t)
      :config
        (setq-default evil-escape-key-sequence "df"
                      evil-escape-delay 0.2))

    ;; evil-magit will be here

    (use-package evil-snipe
      :diminish evil-snipe-local-mode
      :init
        (setq evil-snipe-smart-case t
              evil-snipe-override-evil-repeat-keys nil)
        (evil-snipe-mode +1)
        (evil-snipe-override-mode +1))

    (use-package evil-commentary
      :after eyebrowse ;; Prevent to override `gc' in visual mode
      :init (evil-commentary-mode))

    ;; Search from a visual selection by pressing "*"
    (use-package evil-visualstar
      :init (global-evil-visualstar-mode))

    ;; Jump between matched tags by pressing "%"
    (use-package evil-matchit
      :init (global-evil-matchit-mode 1))

    ;; Align objects with 'gl MOTION CHAR' and 'gL MOTION CHAR'
    (use-package evil-lion
      :init (evil-lion-mode))

    ;; I didn't find it in package archives, so let's use it as a file
    (require 'evil-textobj-line)

    ;; (use-package evil-textobj-anyblock
    ;;   :config
    ;;     (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
    ;;     (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

    (use-package evil-collection
      :init
        (setq evil-collection-mode-list '(company custom magit))
        (evil-collection-init))

) ;; use-package evil ends here

(use-package reverse-im
  :config (eval-when-compile (reverse-im-activate "russian-computer")))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :bind (
    ("M-h" . drag-stuff-left)
    ("M-j" . drag-stuff-down)
    ("M-k" . drag-stuff-up)
    ("M-l" . drag-stuff-right))
  :config
    (drag-stuff-global-mode 1))

(use-package magit
  :config
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package diff-hl
  :config
    (diff-hl-flydiff-mode 1)
    (global-diff-hl-mode 1))

(use-package zoom
  :diminish zoom-mode)

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :ensure t
  :init (global-highlight-parentheses-mode)
  :config
    (setq hl-paren-background-colors '("#5b5039"))
    (setq hl-paren-colors nil)
    (add-hook 'evil-visual-state-entry-hook
              (lambda ()
                (setq hl-paren-background-colors nil)
                (setq hl-paren-colors '("#f0c674"))
                (with-no-warnings
                  (hl-paren-color-update))))
    (add-hook 'evil-visual-state-exit-hook
              (lambda ()
                (setq hl-paren-background-colors '("#5b5039"))
                (setq hl-paren-colors nil)
                (with-no-warnings
                  (hl-paren-color-update))))
) ;; use-package highlight-parentheses ends here

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    ("C-w" . ivy-backward-kill-word))
  :config
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-count-format "[%d/%d] "))

(use-package counsel
  :config
    (defun justf/counsel-yank-pop-replace-region (&optional arg)
      "Delete the region before inserting poped string."
      (when (and evil-mode (eq 'visual evil-state))
        (kill-region (region-beginning) (region-end))))

    (advice-add 'counsel-yank-pop :before 'justf/counsel-yank-pop-replace-region))

(use-package projectile
  :config (projectile-mode 1))

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
    ("C-j" . company-select-next)
    ("C-k" . company-select-previous))
    ;; ("TAB" . company-complete-selection))
  :config
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil)

    (setq company-dabbrev-downcase nil
          company-dabbrev-code-everywhere t
          company-dabbrev-code-modes t
          company-dabbrev-code-ignore-case t
          company-transformers '(company-sort-by-occurrence
                                company-sort-by-backend-importance))

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))




    (defun justf/shell-mode-hook ()
      (set (make-local-variable 'company-backends)
              '((company-capf company-files :with company-yasnippet)
                (company-dabbrev-code company-dabbrev))))
    (add-hook 'shell-mode-hook 'justf/shell-mode-hook)

    (add-hook 'prog-mode-hook (lambda ()
                    (add-to-list 'company-backends 'company-dabbrev-code)
                    (add-to-list 'company-backends 'company-dabbrev)))


    (use-package company-quickhelp
      :config (company-quickhelp-mode))

    (use-package company-shell
      :config (add-to-list 'company-backends 'company-shell))

    ;; (use-package company-web
    ;;   :config (add-to-list 'company-backends 'company-web-html))

    ;; (use-package company-css
    ;;   :config (add-to-list 'company-backends 'company-css))

    ;; (use-package company-tern
    ;;   :config
    ;;     (add-to-list 'company-backends 'company-tern)
    ;;     (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    ;;     (add-hook 'web-mode-hook (lambda () (tern-mode t))))

    (use-package company-elisp
      :init (add-to-list 'company-backends 'company-elisp))

    (use-package company-flx ;; fuzzy completions
      :init (company-flx-mode +1))

    (use-package company-jedi
      :config
        (setq jedi:environment-virtualenv
          (list (expand-file-name "~/.emacs.d/.python-environments/")))
        (add-hook 'python-mode-hook 'jedi:setup)
        (setq jedi:complete-on-dot t)
        (setq jedi:use-shortcuts t)
        (defun config/enable-company-jedi ()
            (add-to-list 'company-backends 'company-jedi))
        (add-hook 'python-mode-hook 'config/enable-company-jedi))
) ;; use-package company ends here

;; (use-package dumb-jump
;;   :bind (("C-." . dumb-jump-go)
;;          ("C-," . dumb-jump-back))
;;   :config
;;     (setq dumb-jump-selector 'ivy)
;;     (setq dumb-jump-default-project "/web")
;;     (dumb-jump-mode))

(use-package elpy
  ;; :diminish elpy-mode
  :config
    (setenv "PATH" (concat (getenv "PATH") ":/home/justf/.local/bin"))
    (setq exec-path (append exec-path '("/home/justf/.local/bin")))
    (elpy-enable)
    (setq elpy-modules '(elpy-module-company elpy-module-eldoc elpy-module-yasnippet elpy-module-sane-defaults))
    (use-package py-autopep8))


(use-package dim
  :config
    (dim-major-name 'vue-html-mode "HTML+Vue"))

(use-package shackle
  :ensure t
  :config
    (setq shackle-rules
      '(("*Help*" :align t :select t)
        ("*Flycheck errors*" :align t :select t :size 0.2)
        ("*Google Translate*" :align t :select t)
        ("*Python Doc*" :align t :select t))
        ;;   "\\`\\*magit-diff: .*?\\'") :regexp t :noselect t)
        ;; ("\\`\\*cider-repl .*" :regexp t :align t :size 0.2)
        ;; ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))
       shackle-default-rule '(:select t)
       shackle-default-size 0.4
       shackle-inhibit-window-quit-on-same-windows t)
    (shackle-mode)
) ;; use-package shackle ends here

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  ;; :commands flycheck-buffer
  :bind (:map flycheck-error-list-mode-map
    ("j" . flycheck-error-list-next-error)
    ("k" . flycheck-error-list-previous-error)
    ("RET" . flycheck-error-list-goto-error))
  :config

    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

    (setq flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          (`not-checked nil)
          (`no-checker "-")
          (`running (propertize "E:-  W:-" 'face 'mode-line-dark))
          (`errored (propertize "!" 'face 'error))
          (`finished
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'error)
                              (no-warnings 'warning)
                              (t 'mode-line-dark))))
             (propertize (format "E:%s  W:%s" (or no-errors 0) (or no-warnings 0))
                         'face face)))
          (`interrupted "-")
          (`suspicious '(propertize "?" 'face 'warning)))))

    ;; Override default flycheck triggers
    (defvar flycheck-check-syntax-triggers '(mode-enabled save idle-change))
    (setq flycheck-emacs-lisp-load-path 'inherit
          flycheck-check-syntax-automatically flycheck-check-syntax-triggers)

    (add-hook 'evil-insert-state-entry-hook
                (lambda ()
                  (setq flycheck-check-syntax-automatically nil)))
    (add-hook 'evil-insert-state-exit-hook
                (lambda ()
                  (setq flycheck-check-syntax-automatically flycheck-check-syntax-triggers)
                  (with-no-warnings
                    (flycheck-buffer))))

    (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)

) ;; use-package flycheck ends here

;; (use-package lsp-mode
;;   :config

;;     (use-package lsp-ui
;;       :config
;;         (setq lsp-ui-sideline-ignore-duplicate t)
;;         (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;     (use-package company-lsp
;;       :after (company lsp-mode)
;;       :config (push 'company-lsp company-backends))
;; )

;; (use-package lsp-mode)

;; (use-package company-lsp
;;   :after (lsp-mode company-mode)
;;   :init (push 'company-lsp company-backends))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode))

(use-package js2-mode
  :init (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  :config (setq js2-basic-offset 2))

;; (use-package lsp-javascript-typescript
;;   :after company
;;   :config
;;     (defun my-company-transformer (candidates)
;;       (let ((completion-ignore-case t))
;;         (all-completions (company-grab-symbol) candidates)))

;;     (defun my-js-hook nil
;;       (make-local-variable 'company-transformers)
;;       (push 'my-company-transformer company-transformers))

;;     (add-hook 'js2-mode-hook 'my-js-hook)
;;     (add-hook 'js2-mode-hook 'lsp-javascript-typescript-enable))

;; (use-package vue-mode)
;;   :config
;;     (use-package lsp-vue
;;       :config (add-hook 'vue-mode-hook #'lsp-vue-mmm-enable))

;; (use-package vue-mode
;;     :ensure t
;;     :mode "\\.vue\\'"
;;    )

;;   (use-package lsp-vue
;;     :ensure t
;;     :after (vue-mode lsp-mode)
;;     :hook ((vue-mode . lsp-vue-enable) (vue-mode . flycheck-mode))
;;   )

;; (use-package css-mode
;;   :ensure nil
;;   ;; :hook ((css-mode . my-css-mode-setup))
;;   :config
;;     (use-package lsp-css)
;;     (defun my-css-mode-setup ()
;;       (when (eq major-mode 'css-mode)
;;         ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
;;         ;; fires for scss-mode because scss-mode is derived from css-mode)
;;         (lsp-css-enable)))
;;     (add-hook 'css-mode 'my-css-mode-setup)
;;     (setq css-indent-offset 2))



(use-package emmet-mode
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :bind (("TAB"      . emmet-next-edit-point)
        ("<backtab>" . emmet-prev-edit-point))
  :init
    (setq emmet-move-cursor-between-quotes t)
    (add-hook 'vue-mode-hook 'emmet-mode)
    ;; (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode))

;; (use-package web-mode
;;   :ensure t
;;   :mode ("\\.html\\'" "\\.vue\\'")
;;   :config
;;     (setq web-mode-markup-indent-offset 2)
;;     (setq web-mode-css-indent-offset 2)
;;     (setq web-mode-code-indent-offset 2)
;;     (setq web-mode-enable-css-colorization t)
;;     (setq web-mode-content-types-alist
;;           '(("vue" . "\\.vue\\'")))

;;     ;; (defun my-web-mode-hook ()
;;     ;;   "Hook for `web-mode'."
;;     ;;     (set (make-local-variable 'company-backends)
;;     ;;         '(company-tern company-web-html company-css company-yasnippet company-files)))

;;     ;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;;     ;; ;; Enable JavaScript completion between <script>...</script> etc.
;;     ;; (advice-add 'company-tern :before
;;     ;;             #'(lambda (&rest _)
;;     ;;                 (if (equal major-mode 'web-mode)
;;     ;;                     (let ((web-mode-cur-language
;;     ;;                           (web-mode-language-at-pos)))
;;     ;;                       (if (or (string= web-mode-cur-language "javascript")
;;     ;;                               (string= web-mode-cur-language "jsx"))
;;     ;;                           (unless tern-mode (tern-mode))
;;     ;;                         (if tern-mode (tern-mode -1)))))))

;;     ;; (use-package elec-pair
;;     ;;   :config
;;     ;;     ;; disable {} auto pairing in electric-pair-mode for web-mode
;;     ;;     (add-hook
;;     ;;       'web-mode-hook
;;     ;;       (lambda ()
;;     ;;         (setq-local electric-pair-inhibit-predicate
;;     ;;                     `(lambda (c)
;;     ;;                         (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c)))))))


;; ) ;; use-package web-mode ends here

;; ;; TODO: Cleanup that func and hook
;; (defun my/use-eslint-from-node-modules ()
;;   "Use local eslint from node_modules before global."
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package google-translate
  :config (setq google-translate-default-source-language "en"
                google-translate-default-target-language "ru"))

(use-package ranger
  :config
    (setq ranger-cleanup-on-disable t
          ranger-cleanup-eagerly t)
    (ranger-override-dired-mode t))

(use-package expand-region
  :bind (("C-\\" . er/expand-region))
         ("C-|"  . er/contract-region))

(use-package recentf
  :config
    (setq recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-exclude '("/\\.lyrics/"
                            ".*\\.srt$"
                            "^/tmp\\.*"
                            "COMMIT_EDITMSG\\'"
                            ".*-autoloads\\.el\\'"
                            "[/\\]\\.elpa/"))
    (recentf-mode 1))

(use-package auto-sudoedit
  :diminish auto-sudoedit-mode
  :config (auto-sudoedit-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line-update-function (function juste-line-update-function))
 '(blink-cursor-mode nil)
 '(byte-compile-delete-errors t)
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default)))
 '(dired-listing-switches "-la --group-directories-first")
 '(electric-pair-inhibit-predicate (quote electric-pair-conservative-inhibit))
 '(electric-pair-pairs (quote ((39 . 39) (34 . 34) (8216 . 8217) (8220 . 8221))))
 '(electric-pair-skip-self (quote electric-pair-default-skip-self))
 '(electric-quote-chars (quote (8216 8217 8220 8221)))
 '(enable-recursive-minibuffers t)
 '(evil-mode-line-format nil)
 '(eyebrowse-mode t)
 '(eyebrowse-mode-line-left-delimiter "  [")
 '(fill-column 79)
 '(fringe-mode 16 nil (fringe))
 '(indent-tabs-mode nil)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(line-spacing 0.1)
 '(lsp-ui-doc-border "#151617")
 '(package-selected-packages
   (quote
    (py-autopep8 elpy resize-window eglot company-elisp ag auto-compile origami reverse-im google-translate expand-region counsel-projectile projectile eyebrowse lsp-css lsp-javascript-typescript lsp-ui company-lsp lsp-mode ranger vue-mode company-web evil-lion lorem-ipsum diff-hl company-shell crontab-mode zoom company-php php-mode evil-snipe drag-stuff evil-magit evil-textobj-anyblock dim auto-sudoedit python-mode smooth-scrolling evil-collection diminish flycheck-pos-tip yasnippet-snippets yaml-mode web-mode use-package syslog-mode spaceline smex shackle rainbow-delimiters org-bullets markdown-mode+ linum-relative js2-mode highlight-parentheses flycheck evil-visualstar evil-visual-mark-mode evil-surround evil-org evil-numbers evil-matchit evil-leader evil-escape evil-commentary evil-anzu emmet-mode dumb-jump counsel company-tern company-quickhelp company-jedi company-flx base16-theme ace-jump-mode)))
 '(sentence-end-double-space nil)
 '(split-window-keep-point t)
 '(web-mode-auto-quote-style 2)
 '(web-mode-enable-auto-pairing t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "#f0c674"))))
 '(border ((t (:background "#c5c8c6"))))
 '(company-scrollbar-bg ((t (:background "#282a2e"))))
 '(company-scrollbar-fg ((t (:background "#757876"))))
 '(company-tooltip ((t (:inherit default :background "#282a2e"))))
 '(custom-button ((t (:background "#1d1f21" :foreground "#6d879e" :box (:line-width 1 :color "#6d879e") :height 90))))
 '(custom-button-mouse ((t (:background "#1d1f21" :foreground "#c5c8c6" :box (:line-width 1 :color "#c5c8c6")))))
 '(custom-button-pressed ((t (:background "#585858" :foreground "#c5c8c6" :box (:line-width 1 :color "#c5c8c6") :height 90))))
 '(custom-comment ((t (:background "#282828"))))
 '(custom-group-subtitle ((t (:foreground "#8aebb7"))))
 '(diff-hl-change ((t (:background "#413b47" :foreground "#413b47"))))
 '(diff-hl-delete ((t (:background "#37292b" :foreground "#37292b"))))
 '(diff-hl-insert ((t (:background "#33362b" :foreground "#33362b"))))
 '(error ((t (:foreground "#cc6666" :weight normal))))
 '(evil-ex-info ((t (:foreground "#cc6666" :slant italic))))
 '(evil-ex-substitute-replacement ((t (:foreground "#f0c674"))))
 '(evil-snipe-first-match-face ((t nil)))
 '(evil-snipe-matches-face ((t (:background "#3d4e4d"))))
 '(eyebrowse-mode-line-active ((t (:inherit nil :foreground "#c5c8c6"))))
 '(flyspell-incorrect ((t (:foreground "#cc6666" :underline "#cc6666"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#585858"))))
 '(font-lock-comment-face ((t (:foreground "#585858"))))
 '(font-lock-constant-face ((t (:foreground "#cc6666"))))
 '(font-lock-fic-author-face ((t (:foreground "#c5c8c6" :underline t))))
 '(font-lock-fic-face ((t (:foreground "#f0c674" :weight bold))))
 '(fringe ((t (:background "#191a1c" :foreground "#585858"))))
 '(header-line ((t (:inherit nil :foreground "#b294bb" :height 90))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(ivy-current-match ((t (:background "#1d1f21" :foreground "#f0c674"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#c5c8c6"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "#cc6666"))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "#cc6666"))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "#cc6666"))))
 '(ivy-minibuffer-match-highlight ((t (:foreground "#81a2be"))))
 '(js2-error ((t (:foreground "#cc6666"))))
 '(js2-external-variable ((t (:foreground "#cc6666" :underline t))))
 '(line-number ((t (:background "#1d1f21" :foreground "#585858"))))
 '(line-number-current-line ((t (:background "#282a2e" :foreground "#c5c8c6" :inverse-video nil))))
 '(linum ((t (:background "#191a1c" :foreground "#585858"))))
 '(lsp-ui-doc-background ((t (:background "#282a2e"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:foreground "#757876" :background "#282a2e" :box (:line-width 3 :color "#282a2e") :overline "#151617" :height 80))))
 '(mode-line-buffer-id ((t (:foreground "#c5c8c6"))))
 '(mode-line-highlight ((t (:foreground "#f0c674" :box nil :weight bold))))
 '(mode-line-inactive ((t (:background "#191a1c" :foreground "#585858" :box (:line-width 3 :color "#191a1c") :overline "#151617" :height 80))))
 '(org-column ((t (:background "#f0c674"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#81a2be"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#c5c8c6"))))
 '(org-link ((t (:foreground "#8abeb7" :underline t))))
 '(org-verbatim ((t (:inherit shadow :foreground "#b294bb"))))
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#b294bb" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#81a2be" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8abeb7" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#b5bd68" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#f0c674" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#de935f" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#cc6666" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#969896" :weight bold))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#c5c8c6" :weight bold))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face :foreground "#cc6666"))))
 '(treemacs-directory-face ((t (:inherit font-lock-function-name-face :height 90))))
 '(treemacs-file-face ((t (:inherit default :height 90))))
 '(treemacs-header-face ((t (:foreground "#ffffff" :height 90))))
 '(vertical-border ((((type x tty)) (:foreground "#151617"))))
 '(warning ((t (:foreground "#f0c674" :weight normal))))
 '(widget-field ((t (:background "#282a2e" :box (:line-width 1 :color "#757876"))))))
(put 'dired-find-alternate-file 'disabled nil)
