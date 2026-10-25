;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless)  ; completion
       vertico             ; minibuffer completion

       :ui
       doom
       ;; dashboard
       hl-todo
       modeline
       workspaces

       :editor
       (evil +everywhere)
       fold
       multiple-cursors
       snippets

       :emacs
       dired             ; file manager
       undo              ; persistent undo
       vc                ; version-control basico

       :checkers
       ;; Spell-check con jinx standalone (ver packages.el/config.el).
       ;; El modulo spell de Doom (spell-fu/flyspell) queda desactivado a proposito.

       :tools
       lsp               ; language server protocol
       magit             ; git porcelain
       pdf               ; pdf-tools (para x86 manual y org)

       :term
       vterm

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (cc +lsp)         ; C/C++ con clangd
       emacs-lisp        ; necesario para config de doom
       (ocaml +lsp)      ; OCaml con tuareg + ocaml-lsp-server
       (org +roam)
       (python +lsp)
       sh
       
       :config
       (default +bindings +smartparens))
