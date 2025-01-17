;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Juan Ignacio Raggio"
      user-mail-address "jotaraggio@icloud.com")

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

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5   ; Recolecta basura después de 5s de inactividad
        gcmh-high-cons-threshold (* 128 1024 1024)) ; 128 MB en tareas pesadas
  (gcmh-mode 1))

;; dired config
(use-package async
  :config
  (dired-async-mode 1))
(setq dired-listing-switches "-alh")

(use-package lsp-mode
  :init
  (setq lsp-completion-provider :capf)) ; Usa la capa de completions más rápida.

(setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil)

;; (use-package corfu
;;   :init
;;   (setq corfu-cycle t           ; permite navegar en ciclos
;;         corfu-auto t            ; completa automáticamente
;;         corfu-auto-delay 0.2    ; reduce el tiempo de espera
;;         corfu-auto-prefix 2)    ; requiere mínimo 2 caracteres
;;   (global-corfu-mode))

;; Custom cursor
(setq evil-normal-state-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq evil-replace-state-cursor 'box)
(setq evil-visual-state-cursor 'box)
(add-hook 'after-change-major-mode-hook
          (lambda () (setq cursor-type 'box)))
(setq cursor-in-non-selected-windows 'box)


(setq doom-font (font-spec :family "Comic mono" :size 14.5 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Comic mono" :size 14.5))

(setq-default line-spacing 4)


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-tokyo-night)


;; Function to change automaticalle themes depending of the hour
;; (defun my/theme-based-on-time ()
;;   "Cambia el tema de Emacs según la hora del día."
;;   (let* ((hour (string-to-number (format-time-string "%H")))
;;          (is-night (or (>= hour 19) (< hour 7))))
;;     (if is-night
;;         (load-theme doom-theme 'doom-tokyo-night t)
;;       (load-theme doom-theme 'doom-solarized-light t))))

;; (my/theme-based-on-time)
;; (run-at-time "00:00" 3600 #'my/theme-based-on-time)


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

(setq font-lock-maximum-decoration '((c-mode . 1) (c++-mode . 1) (java-mode . 1)))
(setq font-lock-maximum-size nil)
(setq jit-lock-defer-time 0.05)  ;; Defer processing slightly

(global-so-long-mode 1)


;; LSP
;; C/C++ LSP
(after! lsp-mode
  (setq lsp-clients-clangd-args '("--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--background-index")))

(after! lsp-mode
  (require 'dap-cpptools)
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl))
  (setq dap-cpptools-extension-version "1.14.0")) ;; Versión estable

(after! dap-mode
  (dap-register-debug-template
   "C++ Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "C++ Debug"
         :MIMode "lldb"
         :program "~/wokspace/Proyects/conways-life-game/a.out"
         :args []
         :cwd "~/workspace/Proyects/conways-life-game/"
         :stopAtEntry t
         :environment []
         :externalConsole t)))

(map! :leader
      (:prefix ("d" . "debug")
       ;;:desc "Start GDB" "g" #'realgud:gdb
       :desc "Start LLDB" "l" #'realgud:lldb
       :desc "DAP Debug" "d" #'dap-debug))


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
