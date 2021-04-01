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

(setq zenburn-scale-org-headlines t)
(setq zenburn-override-colors-alist
      '(
;;         ;; ("zenburn-bg+3" . "#425af5")) ;; line nos
;;         ("zenburn-bg-08" . "#000000") ;; background
;;         ;; ("zenburn-bg" . "#425af5")) ;;current line
        ("zenburn-bg-1"     . "#0739b8")) ;;mark highlighter
      )
(load-theme 'zenburn t)

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

