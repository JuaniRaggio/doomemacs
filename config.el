;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; =============================================================================
;; INFO PERSONAL
;; =============================================================================
(setq user-full-name "Juan Ignacio Raggio"
      user-mail-address "jgarciavautrinraggi@itba.edu.ar")
(setq epa-file-encrypt-to "jgarciavautrinraggi@itba.edu.ar")
(setq epa-file-decrypt-to "jgarciavautrinraggi@itba.edu.ar")

;; =============================================================================
;; OPTIMIZACIONES DE RENDIMIENTO
;; =============================================================================

;; GC optimizada con gcmh
(after! gcmh
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)
        gcmh-low-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.2))

;; Archivos grandes
(setq large-file-warning-threshold (* 50 1024 1024))

;; Reducir redisplay para evitar freezes
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil)

;; Font caches
(setq inhibit-compacting-font-caches t)

;; Subprocess output (3 MB para LSP)
(setq read-process-output-max (* 3 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bidirectional text desactivado
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Auto-save conservador
(setq auto-save-interval 300
      auto-save-timeout 60)

;; Auto-revert (intervalo relajado para menos overhead)
(global-auto-revert-mode 1)
(setq auto-revert-interval 3
      auto-revert-check-vc-info nil
      global-auto-revert-non-file-buffers t)

;; JIT lock - fontificacion lazy
(setq jit-lock-defer-time 0.05
      jit-lock-stealth-time 1.0
      jit-lock-stealth-nice 0.1
      jit-lock-chunk-size 2000)

;; so-long para archivos enormes
(global-so-long-mode 1)

;; =============================================================================
;; PATH DESDE SHELL (simplificado - solo PATH, no todas las variables)
;; =============================================================================
(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; =============================================================================
;; APARIENCIA
;; =============================================================================
(setq doom-font (font-spec :family "Sometype Mono" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Sometype Mono" :size 16))
(setq doom-theme 'doom-oksolar-dark)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq-default tab-width 4)
(setq-default line-spacing 4)
(setq scroll-margin 8)
(blink-cursor-mode 1)

;; Hangul fonts
(set-fontset-font t 'hangul
                  (font-spec :family "Noto Sans CJK KR" :size 22))

;; Maximizar frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Guardar posicion en archivo
(save-place-mode 1)

;; Comentar con SPC #
(map! :leader "#" #'comment-line)

;; =============================================================================
;; CORFU - COMPLETION
;; =============================================================================
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 10
        corfu-quit-no-match 'separator))

;; TAB/C-y acepta completion, C-n/C-p navega, ENTER inserta newline
(after! corfu
  (evil-make-overriding-map corfu-map 'insert)
  (define-key corfu-map (kbd "C-y") #'corfu-insert)
  (define-key corfu-map (kbd "C-n") #'corfu-next)
  (define-key corfu-map (kbd "C-p") #'corfu-previous)
  (define-key corfu-map (kbd "TAB") #'corfu-insert)
  (define-key corfu-map [tab] #'corfu-insert)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map [return] nil))

;; Cape - fuentes adicionales de completion (al final para no pisar LSP)
(after! cape
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

;; =============================================================================
;; VERTICO / CONSULT (busqueda tipo telescope)
;; =============================================================================
(after! vertico
  (setq vertico-count 15
        vertico-cycle t))

(after! consult
  (setq consult-preview-key 'any
        consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden ."))

(after! orderless
  (setq orderless-matching-styles '(orderless-flex orderless-regexp)))

;; =============================================================================
;; SNIPPETS
;; =============================================================================
(after! yasnippet
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                '("~/.doom.d/snippets")))
  (setq yas-triggers-in-field t))

;; =============================================================================
;; ORG MODE
;; =============================================================================
(setq org-directory "~/Notes")

;; Org-superstar: bullets bonitos
(use-package! org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "▶" "▷" "◆" "◇"))
  (setq org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?•))))

(after! org
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-with-latex-preview nil
        org-log-done 'time)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))

  (setq org-agenda-files '("~/Notes/roamnotes"))

  ;; Org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
     (C . t)))

  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  (setq org-babel-default-header-args:python
        '((:results . "output replace")
          (:exports . "both")))

  (setq python-shell-completion-native-enable nil)

  ;; PDFs en emacs
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.pdf\\'" . emacs))))

;; Python para org-babel
(setq python-shell-interpreter "/opt/homebrew/bin/python3")
(after! org
  (setq org-babel-python-command "/opt/homebrew/bin/python3"))

;; Visual fill column para escritura centrada
(defun my/org-mode-visual-fill ()
  (setq-local visual-fill-column-width 130
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Line numbers en org
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 1)))

;; =============================================================================
;; ORG-ROAM
;; =============================================================================
(after! org-roam
  (setq org-roam-directory "~/Notes/roamnotes"
        org-roam-completion-everywhere t
        ;; Evitar GC durante operaciones de DB (previene micro-freezes)
        org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-autosync-mode))

;; Keybindings para org-roam
(map! :leader
      (:prefix ("n" . "notes")
       :desc "Toggle roam buffer"  "l" #'org-roam-buffer-toggle
       :desc "Find node"           "f" #'org-roam-node-find
       :desc "Insert node"         "i" #'org-roam-node-insert
       :desc "Roam graph"          "g" #'org-roam-graph
       :desc "Capture"             "c" #'org-roam-capture
       :desc "Add tag"             "t" #'org-roam-tag-add
       :desc "Add alias"           "a" #'org-roam-alias-add
       :desc "Dailies today"       "d" #'org-roam-dailies-goto-today))

;; =============================================================================
;; PDF-TOOLS
;; =============================================================================
(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1
        pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(add-hook 'pdf-view-mode-hook
          (lambda () (display-line-numbers-mode -1)))

;; =============================================================================
;; ASM / x86
;; =============================================================================
(use-package! x86-lookup
  :config
  (setq x86-lookup-pdf "~/workspace/itbaworkspace/arquitectura/Assemblerx86_64/docs/325462-088-sdm-vol-1-2abcd-3abcd-4.pdf"))

(global-set-key (kbd "C-c x") 'x86-lookup)

;; =============================================================================
;; DIRED
;; =============================================================================
(use-package! async
  :config
  (dired-async-mode 1))
