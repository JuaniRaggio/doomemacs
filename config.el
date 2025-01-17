;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

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

;; Custom cursor
(setq evil-normal-state-cursor 'box)
(setq evil-insert-state-cursor 'box)
(setq evil-replace-state-cursor 'box)
(setq evil-visual-state-cursor 'box)
(add-hook 'after-change-major-mode-hook
          (lambda () (setq cursor-type 'box)))
(setq cursor-in-non-selected-windows 'box)


(setq doom-font (font-spec :family "monaco" :size 14.5 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "monaco" :size 14.5))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Cool themes
;; Light mode:
;; (setq doom-theme 'doom-plain)
;; (setq doom-theme 'doom-earl-grey)
;;
;; Dark mode:
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-tokyo-night)

(defun my/theme-based-on-time ()
  "Cambia el tema de Emacs según la hora del día."
  (let* ((hour (string-to-number (format-time-string "%H")))
         (is-night (or (>= hour 19) (< hour 7))))
    (if is-night
        (load-theme 'doom-tokyo-night t)
      (load-theme 'doom-plain t))))

(my/theme-based-on-time)
(run-at-time "00:00" 3600 #'my/theme-based-on-time)


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

;; LSP
;; C/C++ LSP
(after! lsp-mode
  (setq lsp-clients-clangd-args '("--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--background-index")))

;; Configuración de JAVA_HOME
(setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@23")

;; Configuración específica de lsp-java
(after! lsp-java
  (setq lsp-java-java-path "/opt/homebrew/opt/openjdk@23/bin/java"
        lsp-java-jdtls-path "/opt/homebrew/opt/jdtls/bin/jdtls"
        lsp-java-configuration-runtimes
        '[(:name "JavaSE-23"
           :path "/opt/homebrew/opt/openjdk@23"
           :default t)]))
;; Opcional: Configurar atajos y otras preferencias
(map! :map java-mode-map
      :localleader
      :desc "Run project" "r" #'lsp-java-build-project
      :desc "Organize imports" "o" #'lsp-java-organize-imports
      :desc "Format buffer" "=" #'lsp-format-buffer)

