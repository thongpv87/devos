;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Thong Pham"
      user-mail-address "thongpv87@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-ayu-light)
;; (setq doom-theme 'doom-tomorow-light)
(setq doom-theme 'doom-gruvbox-light)
(setq doom-themes-treemacs-theme "doom-colors")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; LSP
(use-package! lsp-haskell
  :defer
  :config
  (setq lsp-haskell-server-path "haskell-language-server"))
;; lsp key bindings
(evil-define-key 'normal lsp-mode-map (kbd "`") lsp-command-map)
(setq!
 lsp-enable-semantic-highlighting t
 lsp-enable-completion-at-point t
 lsp-lens-enable t
 lsp-enable-imenu t
 lsp-enable-indentation t
 lsp-enable-symbol-highlighting t
 lsp-enable-text-document-color t
 lsp-headerline-breadcrumb-enable t
 lsp-signature-auto-activate t
 lsp-signature-render-documentation t
 lsp-haskell-plugin-retrie-global-on nil
 lsp-haskell-plugin-tactics-global-on nil

 lsp-ui-sideline-enable t
 lsp-ui-sideline-show-hover nil
 lsp-ui-sideline-ignore-duplicate t
 lsp-ui-sideline-show-symbol t
 lsp-ui-sideline-show-code-actions t
 lsp-ui-sideline-show-diagnostics t

 lsp-ui-doc-enable t
 lsp-ui-doc-use-webkit nil
 lsp-ui-doc-enhanced-markdown t
 lsp-ui-doc-use-childframe t
 lsp-ui-doc-include-signature t
 lsp-ui-doc-show-with-cursor t
 lsp-ui-doc-show-with-mouse t)


;; TREEMACS
(setq winum-scope 'visible)
(map!
 "M-0" #'treemacs-select-window
 "M-1" #'winum-select-window-1
 "M-2" #'winum-select-window-2
 "M-3" #'winum-select-window-3
 "M-4" #'winum-select-window-4
 "M-5" #'winum-select-window-5
 "M-6" #'winum-select-window-6
 "M-7" #'winum-select-window-7
 "M-8" #'winum-select-window-8
 "M-9" #'winum-select-window-9)

;; OTHERS
(add-hook! treemacs-mode
           (treemacs-load-theme "all-the-icons"))
