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

;; Comentar region o linea con SPC #
(map! :leader
      "#" #'comment-line)

;; =============================================================================
;; IMPORTAR VARIABLES DE ENTORNO DESDE LA SHELL DEL OS
;; =============================================================================
;; Esto soluciona el problema de que Emacs GUI en macOS no hereda PATH, etc.
(use-package! exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  ;; Importar TODAS las variables de entorno
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    ;; Copiar todas las variables del shell
    (let ((envs (shell-command-to-string
                 (concat (exec-path-from-shell--shell) " -l -i -c 'env'"))))
      (dolist (line (split-string envs "\n" t))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let ((var (match-string 1 line))
                (val (match-string 2 line)))
            (unless (member var '("_" "PWD" "SHLVL" "TERM" "TERM_PROGRAM"))
              (setenv var val))))))))

;; =============================================================================
;; OPTIMIZACIONES DE RENDIMIENTO (Consolidado)
;; =============================================================================

;; Garbage Collection optimizada con gcmh
(after! gcmh
  (setq gcmh-idle-delay 'auto  ; default es auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)  ; 64 MB (mas estable que 128)
        gcmh-low-cons-threshold (* 16 1024 1024)   ; 16 MB
        gc-cons-percentage 0.2))

;; Optimizacion para archivos grandes
(setq so-long-threshold 20000)
(setq large-file-warning-threshold (* 50 1024 1024))  ; 50 MB

;; Reducir redisplay para evitar freezes
(setq redisplay-skip-fontification-on-input t)
(setq fast-but-imprecise-scrolling t)
(setq auto-window-vscroll nil)

;; Inhibir compactacion de fuentes (reduce lag)
(setq inhibit-compacting-font-caches t)

;; Proceso de subprocesos mas eficiente
(setq read-process-output-max (* 3 1024 1024))  ; 3 MB (importante para LSP)
(setq process-adaptive-read-buffering nil)

;; Bidirectional text - desactivar si no usas hebreo/arabe
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Reducir frecuencia de auto-save para evitar micro-freezes
(setq auto-save-interval 300)  ; cada 300 caracteres
(setq auto-save-timeout 60)    ; cada 60 segundos idle

;; =============================================================================
;; BUSQUEDA TIPO FZF/TELESCOPE
;; =============================================================================
(after! vertico
  (setq vertico-count 15)             ; Mostrar 15 resultados
  (setq vertico-cycle t))             ; Ciclar resultados

(after! consult
  ;; Preview automatico al navegar resultados
  (setq consult-preview-key 'any)
  ;; Usar ripgrep para busquedas (mas rapido)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden ."))

;; Orderless - busqueda fuzzy estilo fzf (separar terminos con espacio)
(after! orderless
  (setq orderless-matching-styles '(orderless-flex orderless-regexp)))

;; =============================================================================
;; CORFU - COMPLETION RAPIDO TIPO IDE
;; =============================================================================
(after! corfu
  (setq corfu-auto t)                 ; Completions automaticos
  (setq corfu-auto-delay 0.1)         ; Delay corto (0.1s)
  (setq corfu-auto-prefix 2)          ; Mostrar despues de 2 caracteres
  (setq corfu-cycle t)                ; Ciclar entre opciones
  (setq corfu-preselect 'prompt)      ; No preseleccionar
  (setq corfu-count 10)               ; Mostrar 10 candidatos max
  (setq corfu-scroll-margin 3)
  (setq corfu-quit-no-match 'separator)) ; Cerrar si no hay match

;; TAB acepta completion, ENTER inserta newline
(map! :after corfu
      :map corfu-map
      "TAB" #'corfu-insert
      [tab] #'corfu-insert
      "RET" nil
      [return] nil)

;; Cape - fuentes adicionales de completion
(after! cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; =============================================================================
;; SNIPPETS PERSONALIZADOS
;; =============================================================================
;; Directorio para tus snippets custom (crear si no existe)
;; Estructura: ~/.doom.d/snippets/<modo>/<nombre-snippet>
;; Ejemplo: ~/.doom.d/snippets/python-mode/pandas-df
(after! yasnippet
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                '("~/.doom.d/snippets")))  ; Tu directorio de snippets
  ;; Expandir snippets con TAB
  (setq yas-triggers-in-field t))

;; LSP optimizado para reducir freezes (pero con documentacion)
(after! lsp-mode
  (setq lsp-completion-provider :none)      ; Doom usa su propio sistema
  (setq lsp-idle-delay 0.5)                 ; Delay antes de procesar (reduce carga)
  (setq lsp-log-io nil)                     ; Desactivar logging IO
  (setq lsp-enable-file-watchers nil)       ; Desactivar file watchers (causa freezes)
  (setq lsp-enable-folding nil)             ; No usar folding de LSP
  (setq lsp-enable-text-document-color nil) ; Desactivar colores de documento
  (setq lsp-enable-on-type-formatting nil)  ; No formatear mientras escribes
  (setq lsp-semantic-tokens-enable nil)     ; Tokens semanticos pueden ser pesados
  (setq lsp-lens-enable nil)                ; CodeLens puede causar lag
  (setq lsp-modeline-diagnostics-enable nil); Diagnosticos en modeline causan updates
  (setq lsp-modeline-code-actions-enable nil)
  ;; Documentacion activada
  (setq lsp-signature-auto-activate t)      ; Mostrar firma de funciones
  (setq lsp-signature-render-documentation t) ; Mostrar docs en firma
  (setq lsp-eldoc-enable-hover t)           ; Hover docs en eldoc
  (setq lsp-eldoc-render-all t))            ; Mostrar toda la info disponible

;; UI para documentacion
(after! lsp-ui
  (setq lsp-ui-doc-enable t)                ; Popup de documentacion
  (setq lsp-ui-doc-show-with-cursor t)      ; Mostrar al pasar cursor
  (setq lsp-ui-doc-delay 0.5)               ; Delay de 0.5s
  (setq lsp-ui-doc-position 'at-point)      ; Posicion del popup
  (setq lsp-ui-sideline-enable nil)         ; Desactivar sideline (causa lag)
  (setq lsp-ui-peek-enable t))              ; Peek definitions

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
(setq-default tab-width 4)
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

  ;; =============================================================================
  ;; ORG-BABEL PYTHON CONFIGURATION
  ;; =============================================================================
  ;; Activar lenguajes en org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
     (C . t)))

  ;; No pedir confirmacion para ejecutar codigo (opcional, quitar si quieres confirmar)
  (setq org-confirm-babel-evaluate nil)

  ;; Mostrar imagenes inline despues de ejecutar
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; Python en org-babel (sin sesion por defecto - las sesiones tienen bugs)
  ;; Para usar sesion: agregar :session python al bloque
  (setq org-babel-default-header-args:python
        '((:results . "output replace")
          (:exports . "both")))

  ;; Fix para sesiones cuando se usen
  (setq python-shell-completion-native-enable nil))

;; =============================================================================
;; PYTHON / VIRTUAL ENVIRONMENT PARA ORG-BABEL
;; =============================================================================
;; Cambiar esta ruta a tu virtual environment con pandas instalado
;; Ejemplo: "~/mi-proyecto/venv/bin/python"
(defvar my/org-babel-python-command "/opt/homebrew/bin/python3"
  "Python interpreter para org-babel. Cambiar a la ruta del venv deseado.")

;; Para sesiones, se usa python-shell-interpreter
(setq python-shell-interpreter my/org-babel-python-command)
(after! org
  (setq org-babel-python-command my/org-babel-python-command))

;; Funcion para cambiar Python/venv interactivamente
(defun my/set-org-babel-python (python-path)
  "Cambiar el interprete de Python para org-babel y sesiones."
  (interactive "fSeleccionar interprete Python: ")
  (setq org-babel-python-command python-path)
  (setq python-shell-interpreter python-path)
  (setq my/org-babel-python-command python-path)
  ;; Matar sesion existente para usar el nuevo interprete
  (when (get-buffer "*Python*")
    (kill-buffer "*Python*"))
  (message "Org-babel ahora usa: %s" python-path))

;; Funcion para activar un venv rapidamente
(defun my/activate-venv-for-babel (venv-dir)
  "Activar un virtual environment para org-babel.
   VENV-DIR es el directorio del venv (ej: ~/proyecto/venv)"
  (interactive "DDirectorio del venv: ")
  (let ((python-path (expand-file-name "bin/python" venv-dir)))
    (if (file-executable-p python-path)
        (progn
          (setq org-babel-python-command python-path)
          (setq python-shell-interpreter python-path)
          (setq my/org-babel-python-command python-path)
          ;; Matar sesion existente para usar el nuevo interprete
          (when (get-buffer "*Python*")
            (kill-buffer "*Python*"))
          (message "Venv activado: %s" python-path))
      (error "No se encontro Python en: %s" python-path))))

;; Just charge latex with C-c C-x C-l
(setq org-startup-with-latex-preview nil)

;; PDF-tools optimizado (evitar llamar pdf-tools-install en cada archivo)
(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1)
  ;; Renderizado optimizado
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil))  ; imagemagick es mas lento

(add-hook 'pdf-view-mode-hook
          (lambda () (display-line-numbers-mode -1)))

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.pdf\\'" . emacs)))

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

;; Tree-sitter optimizado (Doom ya lo maneja con el modulo tree-sitter)
;; Solo necesitamos asegurar que use highlighting
(after! tree-sitter
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Font-lock optimizado para reducir lag
(setq font-lock-maximum-decoration t)  ; nil puede causar problemas, usar t
(setq jit-lock-defer-time 0.05)        ; Valor bajo para mejor respuesta
(setq jit-lock-stealth-time 1.0)       ; Fontificar en background despues de 1s
(setq jit-lock-stealth-nice 0.1)       ; Pausa entre chunks
(setq jit-lock-chunk-size 2000)        ; Chunks mas pequenos

;; so-long para archivos grandes
(global-so-long-mode 1)


;; asm x86-lookup config
(use-package x86-lookup
  :ensure t
  :config
  ;; Path to Intel manuals
  (setq x86-lookup-pdf "~/workspace/itbaworkspace/arquitectura/Assemblerx86_64/docs/325462-088-sdm-vol-1-2abcd-3abcd-4.pdf"))

(global-set-key (kbd "C-c x") 'x86-lookup)


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

;; Configuración de fuentes para Hangul
(set-fontset-font t 'hangul
                  (font-spec :family "Noto Sans CJK KR" 
                            :size 22))
