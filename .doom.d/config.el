(setq confirm-kill-emacs nil)
(setq neo-smart-open t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "<f9>") 'save-buffer)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key global-map (kbd "C-M-r") 'vr/isearch-backward) ;; C-M-r
(define-key global-map (kbd "C-M-s") 'vr/isearch-forward) ;; C-M-s
(define-key global-map (kbd "C-c C") 'evilnc-comment-and-kill-ring-save)
(setq helm-candidate-number-limit 300)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq org-startup-folded t)
(setq doom-theme 'gruvbox-dark-medium)
(use-package! magit
  :config
  (define-key magit-section-mode-map (kbd "<C-tab>") nil) ;; conflicts with nswbuff
)

(use-package! nswbuff                    ; Quick switching between buffers
  :bind* (("<C-tab>"           . nswbuff-switch-to-next-buffer)
          ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  (:map nswbuff-override-map
	("b" . nswbuff-kill-this-buffer))
  :config (setq nswbuff-display-intermediate-buffers t
                nswbuff-status-window-layout 'minibuffer
                nswbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*" ".*magit.*" "placeholder-file")
                nswbuff-buffer-list-function 'nswbuff-projectile-buffer-list
  ))

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)
(setq magit-ediff-dwim-show-on-hunks t)
(setq +evil-want-o/O-to-continue-comments nil)

(use-package! super-save
  :config
  (super-save-mode +1))

(use-package! realgud-ipdb)

(fset 'breakpoint-below
   (kmacro-lambda-form [escape ?o ?b ?r ?e ?a ?k ?p ?o ?i ?n ?t ?\( ?\) f9 escape] 0 "%d"))
(define-key global-map (kbd "S-<f8>") 'breakpoint-below)
(fset 'breakpoint
   (kmacro-lambda-form [escape ?i ?b ?r ?e ?a ?k ?p ?o ?i ?n ?t ?\( ?\) f9 escape] 0 "%d"))
(define-key global-map (kbd "<f8>") 'breakpoint)

(fset 'clear-vterm
   (kmacro-lambda-form [escape ?i ?\C-a ?\C-k ?c ?l ?e ?a ?r return ?\C-c ?\C-l? ?\C-c ?\C-l] 0 "%d"))
(map! :after vterm
      :map vterm-mode-map
      :ni "<f7>" #'clear-vterm)

(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name))))))

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(use-package! vterm
  :config
  (set-popup-rule! "^vterm" :ignore t))


(map! :leader
      :desc "terminal vterm" "o t" #'vterm)

(map! (:map evil-window-map
 "s"       #'+evil/window-split-and-follow
 "v"       #'+evil/window-vsplit-and-follow))

(map! :ie "C-h" #'backward-delete-char-untabify)
(map! (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        "C-h" #'backward-delete-char-untabify)

      (:after evil
        :map evil-ex-completion-map
        "C-h" #'backward-delete-char-untabify)

     (:after ivy
        :map ivy-minibuffer-map
        "C-h" #'backward-delete-char-untabify)
)
(map! :after evil-org
:map evil-org-mode-map
:i "C-h" #'backward-delete-char-untabify)
(map! :after evil-org
:map company-active-map
"C-h" #'backward-delete-char-untabify)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'"))

;; EAF
;; (use-package! eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :init
;;   (use-package! epc :defer t :ensure t)
;;   (use-package! ctable :defer t :ensure t)
;;   (use-package! deferred :defer t :ensure t)
;;   (use-package! s :defer t :ensure t)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   :config
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
;; (require 'eaf-evil)

;; (define-key key-translation-map (kbd "SPC")
;;     (lambda (prompt)
;;       (if (derived-mode-p 'eaf-mode)
;;           (pcase eaf--buffer-app-name
;;             ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
;;                            (kbd "SPC")
;;                          (kbd eaf-evil-leader-key)))
;;             ("pdf-viewer" (kbd eaf-evil-leader-key))
;;             ("image-viewer" (kbd eaf-evil-leader-key))
;;             (_  (kbd "SPC")))
;;         (kbd "SPC"))))
;; (dolist (theme custom-enabled-themes)
;;   (disable-theme theme))
;; (disable-theme 'doom-one)
;; (disable-theme 'solaire-swap-bg-theme)


(setq japanese-mode-enabled nil)
(defun toggle-japanese-mode ()
    (interactive)
    (if japanese-mode-enabled
        (progn
          (evil-exit-emacs-state)
          (mozc-mode)
          (message "japanese mode disabled")
          (setq japanese-mode-enabled nil)
          )
      (progn
        (evil-emacs-state)
        (mozc-mode)
        (setq japanese-mode-enabled t)
        (message "japanese mode enabled")
        )
     )
)
(define-key global-map (kbd "C-c l") 'toggle-japanese-mode)

(add-hook 'vterm-copy-mode-hook
            (lambda ()

                (if vterm-copy-mode
                        (progn
                        (set-background-color "Grey31")
                        (message "copy mode enabled")
                        )
                (progn
                        (set-background-color "#282828")
                        (message "copy mode disabled")
                        )
                )
                )
            )
(define-key global-map (kbd "S-<left>") 'windmove-left)
(define-key global-map (kbd "S-<right>") 'windmove-right)
(define-key global-map (kbd "S-<up>") 'windmove-up)
(define-key global-map (kbd "S-<down>") 'windmove-down)


(use-package! treemacs
  :config
  (treemacs-follow-mode 1)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
