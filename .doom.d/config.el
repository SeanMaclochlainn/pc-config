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
(setq helm-candidate-number-limit 300)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(use-package! nswbuff                    ; Quick switching between buffers
  :bind* (("<C-tab>"           . nswbuff-switch-to-next-buffer)
          ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
  (:map nswbuff-override-map
	("b" . nswbuff-kill-this-buffer))
  :config (setq nswbuff-display-intermediate-buffers t
                nswbuff-status-window-layout 'minibuffer
                nswbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*")
  ))


(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)
(setq magit-ediff-dwim-show-on-hunks t)

(load-theme 'doom-monokai-classic t)
(use-package! super-save
  :config
  (super-save-mode +1))
(use-package! realgud-ipdb)
