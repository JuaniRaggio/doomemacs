;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Juan Ignacio Raggio"
      user-mail-address "jgarciavautrinraggi@itba.edu.ar")
(setq epa-file-encrypt-to "jgarciavautrinraggi@itba.edu.ar")
(setq epa-file-decrypt-to "jgarciavautrinraggi@itba.edu.ar")
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

;;(setq +doom-dashboard-ascii-banner-fn t)

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5   ; Recolecta basura después de 5s de inactividad
        gcmh-high-cons-threshold (* 128 1024 1024)) ; 128 MB en tareas pesadas
  (gcmh-mode 1))

;; Optimization for larger files
(setq so-long-threshold 30000)

;; Garbage collector will be less agresive so we get a better balance between memory management and performance
(setq gcmh-aggressive-compacting nil)

(use-package lsp-mode
  :init
  (setq lsp-completion-provider :vertico)) ; Usa la capa de completions más rápida.

;; dired config
(use-package async
  :config
  (dired-async-mode 1))

;; Cursor customization
(blink-cursor-mode 1)

(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 15))

;; Space inbetween lines
(setq-default line-spacing 4)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Light theme
;; (setq doom-theme 'doom-plain)
;; Dark theme
(setq doom-theme 'doom-moonlight)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Tabs as spaces tab length tab width
(setq-default tab-width 2)
(setq scroll-margin 8)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes")

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Reemplaza asteriscos con símbolos.
  (setq org-superstar-headline-bullets-list '("◉" "○" "▶" "▷" "◆" "◇"))
  ;; Opcional: resalta las viñetas de las listas también.
  (setq org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?•))))

(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  )

;; Just charge latex with C-c C-x C-l
(setq org-startup-with-latex-preview nil)

;; Pdf view
(setq-default pdf-view-display-size 'fit-width)
(setq pdf-view-resize-factor 1.1)

(add-hook 'pdf-view-mode-hook
          (lambda () (display-line-numbers-mode -1)))

(add-hook 'pdf-view-mode-exit-hook
          (lambda () (display-line-numbers-mode 1)))

(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "\\.pdf\\'" buffer-file-name)
              (pdf-tools-install))))

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.pdf\\'" . emacs)))

;; Open automatically pdf-view-mode when a .pdf file is opened
(setq find-file-visit-truename t)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(defun my/org-mode-visual-fill ()
  (setq-local visual-fill-column-width 130
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Toggle line numbers for org mode
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 1)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;;; Keybind to copy and paste to system clipboard; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Syntax better highlighting and optimizations regex -> tree-sitter
(use-package! tree-sitter
  :hook ((c-mode c++-mode java-mode ruby-mode) . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq font-lock-maximum-decoration nil)

;; If there is lag in big files, this parameter should be increased
(setq jit-lock-defer-time 0.5)  ;; Defer processing slightly

(global-so-long-mode 1)

;; More optimizations
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(setq gcmh-idle-delay 5)  ; Reduces to 3 sec
(setq gcmh-aggressive-compacting t)
(setq gcmh-low-cons-threshold (* 16 1024 1024))  ; 16 MB

;; LSP
;; Toggle auto install language servers
(setq lsp-auto-install-servers nil)

;; C/C++ LSP
(setq lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-clients-clangd-args '("--header-insertion=never"
                                "--header-insertion-decorators=0"
                                "--background-index")
      lsp-clients-clangd-executable "/opt/homebrew/opt/llvm/bin/clangd"
      lsp-headerline-breadcrumb-enable t)

(map! :leader
      (:prefix ("d" . "debug")
       :desc "DAP Debug" "d" #'dap-debug
       :desc "DAP Debug Last" "r" #'dap-debug-restart
       :desc "DAP Debug Recent" "l" #'dap-debug-last))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp)
  (require 'dap-java)
  (setq lsp-java-save-actions-organize-imports t
        lsp-java-format-on-type-enabled t))

(after! lsp-java
  (setq lsp-java-server-install-dir "/Users/juaniraggio/.emacs.d/.local/etc/lsp/eclipse.jdt.ls/"
        lsp-java-workspace-dir "/Users/juaniraggio/.emacs.d/.local/cache/workspace/"
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "Google"))

;; Java commands LSP
(map! :after lsp-java
      :map java-mode-map
      :leader
      (:prefix ("r" . "refactor")
               "r" #'lsp-rename))
