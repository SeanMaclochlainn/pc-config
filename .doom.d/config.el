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
(define-key global-map (kbd "C-x C-j") 'evilnc-comment-and-kill-ring-save)
(setq helm-candidate-number-limit 300)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(background-color . "LightCyan3"))
;; (add-to-list 'default-frame-alist '(cursor-color . "palegoldenrod"))
(setq org-startup-folded t)
;; (disable-theme)
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
;; (add-hook 'after-init-hook
;;           (lambda () (run-with-timer 5 nil #'set-cursor-color "SystemRedColor")))
;; (setq zenburn-scale-org-headlines t)
;; (disable-theme 'doom-one)
;; (setq zenburn-override-colors-alist
;;       '(
;; ;;         ;; ("zenburn-bg+3" . "#425af5")) ;; line nos
;; ;;         ("zenburn-bg-08" . "#000000") ;; background
;; ;;         ;; ("zenburn-bg" . "#425af5")) ;;current line
;;         ("zenburn-bg-1"     . "#0739b8")) ;;mark highlighter
;;       )
;; (load-theme 'zenburn t)
;; (load-theme 'solarized-light t)
;; (load-theme 'manoj-dark)
;; (set-face-attribute 'hl-line nil :inherit nil :background "gray6")

;; (set-face-attribute 'hl-line nil)
;; (load-theme 'dichromacy)

;; (set-face-attribute 'hl-line nil :inherit nil)

;; (set-face-attribute 'hl-line nil :inherit nil :background "gray6")
;; (global-hl-line-mode 1)

;; (custom-set-faces!
;;   '(hl-line nil :inherit nil :background "gray6")
;; )

;; (global-hl-line-mode 1)
;; (set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)

;; (set-face-attribute 'region nil :background "#8fc8db" :foreground 'unspecified)
;; (set-cursor-color "#ffffff")
;; (set-face-attribute 'mouse nil :background "#d9cc59" :foreground 'unspecified)
;; (load-theme "default")
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
