(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36" "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages '(org-roam-ql org-roam))
 '(safe-local-variable-values
   '((lsp-clients-clangd-args "-I/opt/homebrew/include" "-L/opt/homebrew/lib" "-std=c++20")
     (lsp-environment-variables
      ("CPLUS_INCLUDE_PATH" . "/opt/homebrew/include")
      ("LIBRARY_PATH" . "/opt/homebrew/lib")
      ("CXXFLAGS" . "-std=c++20"))
     (projectile-project-name . "rotatingCube")
     (eval setenv "CXXFLAGS" "-std=c++20")
     (eval setenv "LIBRARY_PATH"
      (concat "/opt/homebrew/lib:"
              (getenv "LIBRARY_PATH")))
     (eval setenv "CPLUS_INCLUDE_PATH"
      (concat "/opt/homebrew/include:"
              (getenv "CPLUS_INCLUDE_PATH")))))
 '(warning-suppress-types '((emacs) (defvaralias) (lexical-binding))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'lsp-copilot-login 'disabled t)
