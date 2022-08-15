;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Tomás Solar"
      user-mail-address "tsolar@gmail.com")

(setq doom-theme 'doom-tomorrow-night)

(setq doom-font (font-spec :family "FiraCode" :size 16)
      doom-variable-pitch-font (font-spec :family "FiraSans" :size 16)
      doom-big-font (font-spec :family "FiraCode" :size 30)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font Mono" :size 16)
      )

(setq +doom-dashboard-banner-dir (concat doom-private-dir "banners/"))

(setq display-line-numbers-type t)

(when IS-MAC (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq-default major-mode 'text-mode)

(setq evil-want-fine-undo t)

(+global-word-wrap-mode +1)

(after! highlight-indent-guides
  (setq
   highlight-indent-guides-method     'fill
   highlight-indent-guides-responsive 'top))

(use-package! rainbow-mode
  :hook (
         (lua-mode . rainbow-mode)
         (css-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         )
  )

(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Kill current buffer"   "d"   #'kill-current-buffer
       :desc "Kill current buffer"   "k"   #'kill-current-buffer
       ))

(use-package! projectile
  :init
  (setq
   projectile-enable-caching nil
   projectile-find-dir-includes-top-level t
   ;; projectile-switch-project-action 'counsel-projectile
   ;; projectile-switch-project-action 'magit-status
   ;; counsel-projectile-switch-project-action 'magit-status

   projectile-sort-order 'recently-active

   projectile-globally-ignored-files '("TAGS" "\#*\#" "*~" "*.la"
                                       "*.o" "*.pyc" "*.elc" "*.exe"))
  :hook (
         (text-mode . projectile-mode)
         (prog-mode . projectile-mode)
         (magit-mode . projectile-mode)
         (css-mode . projectile-mode)
         (yaml-mode . projectile-mode)
         (gitignore-mode . projectile-mode)
         )
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  )

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq org-directory "~/org/")

(use-package! ruby-mode
  :mode "\\.\\(?:a?rb\\|axlsx\\)\\'"
  :hook (
         (ruby-mode . subword-mode)
         )
  ;; :config
  ;; (setq ruby-deep-indent-paren nil)
  )

(use-package! ruby-tools
  :hook ((ruby-mode . ruby-tools-mode)
         (slim-mode . ruby-tools-mode))
  :diminish ruby-tools-mode)

(use-package! projectile-rails
  :hook (projectile-mode . projectile-rails-global-mode))

(use-package! yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(setq js-indent-level 2)
(setq typescript-indent-level 2)

(use-package! js2-mode
  :mode "\\.m?js\\'"
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t))

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
    (setq web-mode-enable-block-face nil)
    (setq web-mode-enable-part-face nil)
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

(use-package! pug-mode
  :commands pug-mode
  :init
  (setq pug-tab-width 2)
  (add-hook 'pug-mode-hook (lambda () (electric-indent-local-mode -1)))
  (add-hook 'mmm-pug-mode-submode-hook (lambda () (electric-indent-local-mode -1)))
  )

(when IS-LINUX (setq +latex-viewers '(pdf-tools)))

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    ;; :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package! nginx-mode
  :mode (("/etc/nginx/nginx.conf\\'" . nginx-mode)
         ("/etc/nginx/sites-\\(enabled\\|available\\)/.*\\'" . nginx-mode)))

(use-package! ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package! dotenv-mode
  :mode (("\\.env\\.?.*\\'" . dotenv-mode)))

(use-package! ranger
  :commands ranger
  :init
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden t)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-dont-show-binary t)
  )

(use-package! org-present
  :commands org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . org-present-next)
         ("C-c C-k" . org-present-prev))
  :hook (
         (org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)))
         ))

(use-package! dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner (concat +doom-dashboard-banner-dir "default.png"))  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
