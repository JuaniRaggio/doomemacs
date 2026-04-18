;;; init.el -*- lexical-binding: t; -*-

;; Configuracion minima: org-roam + x86 asm
;; Correr 'doom sync' despues de modificar

(doom! :completion
       (corfu +orderless)  ; completion (sin +icons para menos overhead)
       vertico             ; minibuffer completion (necesario para org-roam)

       :ui
       doom              ; theming base
       doom-dashboard    ; splash screen
       hl-todo           ; highlight TODO/FIXME en org
       modeline          ; barra de estado
       workspaces        ; tab emulation y persistence

       :editor
       (evil +everywhere); vim keybindings
       fold              ; folding en org
       multiple-cursors  ; editing en multiples lugares
       snippets          ; yasnippet

       :emacs
       dired             ; file manager
       undo              ; persistent undo
       vc                ; version-control basico

       :tools
       lsp               ; language server protocol
       magit             ; git porcelain
       pdf               ; pdf-tools (para x86 manual y org)

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (cc +lsp)         ; C/C++ con clangd
       emacs-lisp        ; necesario para config de doom
       (org +roam2)      ; org-mode con org-roam v2
       (python +lsp)     ; python con pyright/pylsp
       sh                ; shell scripts

       :config
       (default +bindings +smartparens))
