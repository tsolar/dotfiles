;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tomás Solar"
      user-mail-address "tsolar@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Inconsolata" :size 18)
      doom-variable-pitch-font (font-spec :family "Inconsolata" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! rbenv
  :init (setq rbenv-show-active-ruby-in-modeline nil)
  :config (progn
            (global-rbenv-mode)
            (add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
            )
  )

(use-package! ruby-tools
  :hook (ruby-mode . ruby-tools-mode)
  :diminish ruby-tools-mode)

;; Wrap lines, please! (I still want to do it the old-fashioned way...)
(+global-word-wrap-mode +1)

(use-package ruby-mode
  :mode   (("Capfile" . ruby-mode)
           ("Gemfile\\'" . ruby-mode)
           ("Rakefile" . ruby-mode)
           ("\\.rb" . ruby-mode)
           ("\\.ru" . ruby-mode)
           ("\\.rake" . ruby-mode)
           ("\\.jbuilder" . ruby-mode)
           ("\\.xlsx\\.axlsx\\'" . ruby-mode))
  :init
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  :hook (
         (ruby-mode . subword-mode)
         )
  :config
  (setq ruby-deep-indent-paren nil))

(use-package! dotenv-mode
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

(when (display-graphic-p)
  (use-package! highlight-indent-guides
    :diminish
    :hook (prog-mode . highlight-indent-guides-mode)
    :init
    (setq highlight-indent-guides-method 'fill)
    (setq highlight-indent-guides-responsive 'top)))

(use-package! rainbow-mode
  :hook (
         (lua-mode . rainbow-mode)
         (css-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         )
  )

(use-package! whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package! pug-mode
  :init
  (setq pug-tab-width 2)
  (add-hook 'pug-mode-hook (lambda () (electric-indent-local-mode -1)))
  (add-hook 'mmm-pug-mode-submode-hook (lambda () (electric-indent-local-mode -1)))
  )

(use-package! ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package! nginx-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("/etc/nginx/nginx.conf\\'" . nginx-mode))
    (add-to-list 'auto-mode-alist '("/etc/nginx/sites-\\(enabled\\|available\\)/.*\\'" . nginx-mode))))

;; (use-package! emmet-mode
;;   :commands (emmet-mode)
;;   :init
;;   (setq emmet-indentation 2)
;;   (setq emmet-move-cursor-between-quotes t) ;; default nil
;;   (setq emmet-expand-jsx-className? t) ;; default nil
;;   (setq emmet-self-closing-tag-style " /") ;; default "/"
;;   :config
;;   (progn
;;     (add-hook 'emmet-mode-hook (lambda ()
;;                                  (setq emmet-preview-default nil)
;;                                  (setq emmet-indentation 2)))
;;     )
;;   :hook (
;;          (sgml-mode . emmet-mode)
;;          (web-mode . emmet-mode)
;;          (css-mode . emmet-mode)
;;          )
;;     )
;;
(setq js-indent-level 2)
(setq typescript-indent-level 2)

(use-package! web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.ctp\\.php\\'" . web-mode)
         ("\\.ctp\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.mjml\\'" . web-mode)
         )
  :init
  ;; (progn
    (setq web-mode-engines-alist
          '(("\\.jinja\\'"  . "django")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)

    ;;(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
    (setq web-mode-disable-auto-pairing nil)
    (setq web-mode-enable-block-face t)
    (setq web-mode-enable-part-face t)
    (setq web-mode-enable-comment-keywords t)
    (setq web-mode-enable-heredoc-fontification t)
    (setq web-mode-disable-css-colorization nil)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-comment-style 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))


    ;; )
  :config
  (progn
    (add-to-list 'web-mode-comment-formats '("javascript" . "// "))
    (add-to-list 'web-mode-comment-formats '("jsx" . "// "))
    (add-to-list 'web-mode-comment-formats '("php" . "// "))

    (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")

    (add-hook 'web-mode-before-auto-complete-hooks
              '(lambda ()
                 (let ((web-mode-cur-language
                        (web-mode-language-at-pos)))
                   (if (string= web-mode-cur-language "php")
                       (yas-activate-extra-mode 'php-mode)
                     (yas-deactivate-extra-mode 'php-mode))
                   (if (string= web-mode-cur-language "css")
                       (setq emmet-use-css-transform t)
                     (setq emmet-use-css-transform nil)))))

    ;; smartparens stuff
    (defun my-web-mode-hook ()
      (setq web-mode-enable-auto-pairing nil))

    (add-hook 'web-mode-hook 'my-web-mode-hook)

    (defun sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
    )
  )

(use-package! js2-mode
  ;; :mode "\\.js$"
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t))

;; (use-package! org-superstar
  ;; :config
  ;; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))
  ;; ))
(setq +latex-viewers '(pdf-tools))

(use-package! ranger
  :init
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden t)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-dont-show-binary t)
  )
