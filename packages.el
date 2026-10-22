;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-superstar)
(package! org-noter)
(package! visual-fill-column)
(package! exec-path-from-shell
  :recipe (:branch "main"))
(package! x86-lookup)
(package! avy)
(package! typst-mode)
(package! google-translate)
(when IS-MAC
  (package! osx-dictionary))

;; Tema gruber-darker (doom-theme en config.el). Hay que declararlo aca para
;; que Doom lo instale en su load-path; instalarlo con M-x package-install NO
;; alcanza (queda solo en custom.el y Doom no lo encuentra al arrancar).
(package! gruber-darker-theme)

;; Migrados desde custom.el (estaban instalados con M-x package-install, que en
;; Doom no alcanza). ros/ros-face: soporte ROS para robotics. agent-shell:
;; shell para agentes de IA. cpputils-cmake: integracion CMake para C++.
(package! ros)
(package! ros-face)
(package! agent-shell)
(package! cpputils-cmake)

;; Spell-check con jinx (via enchant + hunspell ko_KR). Standalone porque
;; el modulo :checkers spell de Doom no integra jinx en esta version.
(package! jinx)

;; valign: alinea tablas org/markdown a nivel de pixel (clave con coreano,
;; que es de doble ancho y rompe la alineacion normal por caracteres)
(package! valign)

(package! d2-mode)

;; org-download: pegar screenshots del portapapeles directo al org.
;; En macOS usa pngpaste (brew install pngpaste) automaticamente.
(package! org-download)

;; org-roam-ui: grafo interactivo (D3) en el browser. Reemplaza al
;; org-roam-graph nativo (graphviz) que queda estirado y no renderiza bien.
;; El :files con "out" es necesario para incluir los assets web compilados.
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

