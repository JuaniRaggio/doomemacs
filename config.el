;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Juan Ignacio Raggio"
      user-mail-address "jgarciavautrinraggi@itba.edu.ar")
(setq epa-file-encrypt-to "jgarciavautrinraggi@itba.edu.ar")
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
;;

(setq +doom-dashboard-ascii-banner-fn nil)

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5   ; Recolecta basura después de 5s de inactividad
        gcmh-high-cons-threshold (* 128 1024 1024)) ; 128 MB en tareas pesadas
  (gcmh-mode 1))

;; Optimization for larger files
(setq so-long-threshold 30000)
;; Garbage collector will be less agresive so we get a better balance between memory management and performance
(setq gcmh-aggressive-compacting nil)

;; dired config
(use-package async
  :config
  (dired-async-mode 1))

;; Custom cursor
(setq evil-normal-state-cursor 'hbar)
(setq evil-insert-state-cursor 'hbar)
(setq evil-replace-state-cursor 'hbar)
(setq evil-visual-state-cursor 'hbar)
(add-hook 'after-change-major-mode-hook
          (lambda () (setq cursor-type 'hbar)))
(setq cursor-in-non-selected-windows 'hbar)
(setq-default cursor-type 'hbar)
(blink-cursor-mode 1)

(setq doom-font (font-spec :family "monaco" :size 14.5 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "monaco" :size 14.5))

;; Space inbetween lines
(setq-default line-spacing 4)


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-tokyo-night)

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

;; Center .org files
(use-package! visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill)
  :config
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t))

(defun my/org-mode-visual-fill ()
  (setq-local visual-fill-column-width 130
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Deactivate line numbers for org mode
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
  :hook ((c-mode c++-mode java-mode) . tree-sitter-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  ;; Activa el resaltado semántico
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq font-lock-maximum-decoration nil)
;;(setq font-lock-maximum-decoration '((c-mode . 1) (c++-mode . 1) (java-mode . 1)))
;;(setq font-lock-maximum-size nil)

;; If there is lag in big files, this parameter should be increased
(setq jit-lock-defer-time 0.5)  ;; Defer processing slightly

(global-so-long-mode 1)

;; More optimizations
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(setq gcmh-idle-delay 5)  ; Reduces to 3 sec
(setq gcmh-aggressive-compacting t)
(setq gcmh-low-cons-threshold (* 16 1024 1024))  ; 16 MB


;; LSP
;; Don't auto install language servers
(setq lsp-auto-install-servers nil)

(use-package lsp-mode
  :init
  (setq lsp-completion-provider :capf)) ; Usa la capa de completions más rápida.

(setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil)

;; C/C++ LSP
(setq lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-idle-delay 0.5
      lsp-clients-clangd-args '("--header-insertion=never"
                                "--header-insertion-decorators=0"
                                "--background-index")
      lsp-clients-clangd-executable "/opt/homebrew/opt/llvm/bin/clangd"
      lsp-headerline-breadcrumb-enable t)

(after! python
        (use-package pyvenv
        :config
        (add-hook 'python-mode-hook
                (lambda ()
                (let ((venv-path (expand-file-name "venv" (projectile-project-root))))
                        (when (file-directory-p venv-path)
                        (pyvenv-activate venv-path)))))))

;; Dap for C/C++
(require 'dap-cpptools)
(setq dap-auto-configure-features '(sessions locals breakpoints expressions repl)
      dap-cpptools-extension-version "1.14.0")

(use-package! dap-mode
  :after lsp-mode
  :config
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions)
        dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-vscode"))

  ;; Debug templates
  (dap-register-debug-template "C++ Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Run C++ Program"
                                     :MIMode "lldb"
                                     :program "${workspaceFolder}/a.out"
                                     :cwd "${workspaceFolder}"
                                     :stopAtEntry t
                                     :environment []
                                     :externalConsole nil
                                     :targetArchitecture "arm64"
                                     )))

(map! :leader
      (:prefix ("d" . "debug")
       :desc "DAP Debug" "d" #'dap-debug
       :desc "DAP Debug Last" "r" #'dap-debug-restart
       :desc "DAP Debug Recent" "l" #'dap-debug-last))


;; Configuración de JAVA_HOME
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp)
  (require 'dap-java)
  (setq lsp-java-save-actions-organize-imports t
        lsp-java-format-on-type-enabled t))

(after! lsp-java
  (setq lsp-java-server-install-dir "~/tools/jdtls"
        lsp-java-workspace-dir "~/workspace/.jdtls"
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "Google"
        lsp-java-save-actions-organize-imports t))

(add-hook 'java-mode-hook #'lsp-deferred)

;; Java commands LSP
;; SPC r r -> Rename any symbol
(map! :after lsp-java
      :map java-mode-map
      :leader
      (:prefix ("r" . "refactor")
               "r" #'lsp-rename))

(map! :after dap-mode
      :leader
      (:prefix ("d" . "debug")
               "d" #'dap-debug
               "b" #'dap-breakpoint-toggle
               "c" #'dap-continue
               "i" #'dap-step-in
               "o" #'dap-step-out
               "n" #'dap-next
               "e" #'dap-eval))

(map! :after lsp-java
      :leader
      (:prefix ("p" . "project")
               "b" #'lsp-java-build-project
               "r" #'lsp-java-run))

(map! :after lsp-java
      :leader
      "e" #'lsp-java-run)

(map! :after lsp-java
      :leader
      (:prefix ("c" . "code actions")
               "o" #'lsp-java-organize-imports
               "f" #'lsp-java-format
               "i" #'lsp-execute-code-action))
