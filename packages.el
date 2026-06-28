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

;; Spell-check con jinx (via enchant + hunspell ko_KR). Standalone porque
;; el modulo :checkers spell de Doom no integra jinx en esta version.
(package! jinx)

;; valign: alinea tablas org/markdown a nivel de pixel (clave con coreano,
;; que es de doble ancho y rompe la alineacion normal por caracteres)
(package! valign)
