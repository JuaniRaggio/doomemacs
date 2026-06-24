;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-superstar)
(package! visual-fill-column)
(package! exec-path-from-shell)
(package! x86-lookup)
(package! avy)
(package! typst-mode)
(package! google-translate)
(package! osx-dictionary)

;; Colorscheme black-metal mayhem (misma paleta base16 que en nvim)
(package! base16-theme
  :recipe (:host github :repo "tinted-theming/base16-emacs"))
