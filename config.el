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

<<<<<<< HEAD
(defun my/disable-company-in-prog-mode ()
  "Desactiva company-mode en buffers de programación."
  (company-mode -1))

(add-hook 'prog-mode-hook #'my/disable-company-in-prog-mode)
(defun my/disable-company-in-terminal ()
  "Desactiva company-mode en eshell y vterm."
  (company-mode -1))

(add-hook 'eshell-mode-hook #'my/disable-company-in-terminal)
(add-hook 'vterm-mode-hook #'my/disable-company-in-terminal)

;; Custom cursor
(setq-default cursor-type 'hbar)
=======
;; Custom cursor
(setq-default cursor-type 'hbar)
(setq blink-cursor-mode 1)
(setq blink-cursor-blinks 10)
>>>>>>> 64a7a03 (Added cursor customization and some features)
(setq evil-normal-state-cursor 'hbar)
(setq evil-insert-state-cursor 'hbar)
(setq evil-replace-state-cursor 'hbar)
(setq evil-visual-state-cursor 'hbar)
(add-hook 'after-change-major-mode-hook
          (lambda () (setq cursor-type 'hbar)))


(setq doom-font (font-spec :family "monaco" :size 14.5 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "monaco" :size 14.5))

<<<<<<< HEAD
=======
;;
>>>>>>> 64a7a03 (Added cursor customization and some features)
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
<<<<<<< HEAD
;;
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
      (load-theme 'doom-earl-grey t))))

;; Ejecuta la función al inicio
(my/theme-based-on-time)

;; Programa una actualización automática cada hora
(run-at-time "00:00" 3600 #'my/theme-based-on-time)

=======
(setq doom-theme 'doom-tokyo-night)
>>>>>>> 64a7a03 (Added cursor customization and some features)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Tabs as spaces tab length tab width
(setq-default tab-width 2)
(setq scroll-margin 8)

;; I finally decided to try using default vim keybinds
(after! evil
<<<<<<< HEAD
  (define-key evil-motion-state-map "j" 'next-line) ; j mueve hacia arriba
  (define-key evil-motion-state-map "k" 'previous-line)     ; k mueve hacia abajo
  (define-key evil-normal-state-map "j" 'next-line)
  (define-key evil-normal-state-map "k" 'previous-line)
  (define-key evil-visual-state-map "j" 'next-line)
  (define-key evil-visual-state-map "k" 'previous-line))
=======
  (define-key evil-motion-state-map "j" 'previous-line) ; j mueve hacia arriba
  (define-key evil-motion-state-map "k" 'next-line)     ; k mueve hacia abajo
  (define-key evil-normal-state-map "j" 'previous-line)
  (define-key evil-normal-state-map "k" 'next-line)
  (define-key evil-visual-state-map "j" 'pervious-line)
  (define-key evil-visual-state-map "k" 'next-line))
>>>>>>> 64a7a03 (Added cursor customization and some features)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")


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
