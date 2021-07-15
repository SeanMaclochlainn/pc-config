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
(setq confirm-kill-processes nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq org-startup-folded t)
(use-package! magit
  :config
  (define-key magit-section-mode-map (kbd "<C-tab>") nil) ;; conflicts with nswbuff
  )

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.env_python3.8\\'"))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'"))


(use-package! nswbuff                    ; Quick switching between buffers
  :bind* (("<C-end>"           . nswbuff-switch-to-next-buffer)
          ("<C-home>" . nswbuff-switch-to-previous-buffer))
  ;; (use-package! nswbuff                    ; Quick switching between buffers
  ;;   :bind* (("<C-tab>"           . nswbuff-switch-to-next-buffer)
  ;;           ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer))
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
;; (define-key global-map (kbd "S-<f8>") 'breakpoint-below)
(fset 'breakpoint
      (kmacro-lambda-form [escape ?i ?b ?r ?e ?a ?k ?p ?o ?i ?n ?t ?\( ?\) f9 escape] 0 "%d"))
(fset 'insert-mode-breakpoint
      (kmacro-lambda-form [?b ?r ?e ?a ?k ?p ?o ?i ?n ?t ?\( ?\)] 0 "%d"))
(define-key global-map (kbd "<f8>") 'breakpoint-below)
(map! :i "C-c b" #'insert-mode-breakpoint)

(fset 'clear-vterm
      (kmacro-lambda-form [escape ?i ?\C-a ?\C-k ?c ?l ?e ?a ?r return ?\C-c ?\C-l? ?\C-c ?\C-l] 0 "%d"))
(map! :after vterm
      :map vterm-mode-map
      :ni "<f7>" #'clear-vterm)
(map! :after vterm
      :map vterm-mode-map
      :i "C-h" #'vterm-send-C-h)
(map! :after vterm
      :map vterm-mode-map
      :i "C-c v" #'vterm-yank)

(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))

(with-eval-after-load 'evil
  ;; (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  ;; (setq-default evil-symbol-word-search t)
  (setq evil-want-fine-undo t)
  (modify-syntax-entry ?_ "w"))

(use-package! vterm
  :config
  (set-popup-rule! "^vterm" :ignore t))

(use-package! treemacs
  :config
  (setq treemacs-follow-mode t))

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
(map! :n "C-t" #'evil-scroll-line-down)
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


(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark)
  (evil-yank start end))

(defun wsl-paste ()
  (interactive)
  (let ((clipboard
         (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    (insert clipboard)))

(defun wsl-delete (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark)
  (evil-delete start end))


;; dd and yy not working with these
;; (define-key evil-visual-state-map (kbd "d") 'wsl-delete)
;; (define-key evil-normal-state-map (kbd "p") 'wsl-paste)
(if (not (string= user-real-login-name "smcloughlin"))
    (progn
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
      ))

(define-key global-map (kbd "C-c c") 'comment-line)
(define-key global-map (kbd "C-c d") 'delete-region)
(global-unset-key (kbd "M-t"))
(define-key global-map (kbd "S-<left>") 'windmove-left)
(define-key global-map (kbd "S-<right>") 'windmove-right)
(define-key global-map (kbd "S-<up>") 'windmove-up)
(define-key global-map (kbd "S-<down>") 'windmove-down)

(map! :leader
      (:prefix ("w" . "window")
       :desc "window right"
       "e" #'windmove-right))

(map! :after evil-org
      :map evil-org-mode-map
      :n
      "S-<left>" #'windmove-left
      "S-<right>" #'windmove-right
      "S-<up>" #'windmove-up
      "S-<down>" #'windmove-down)



;; (unless (display-graphic-p)
;;         ;; activate mouse-based scrolling
;;         (xterm-mouse-mode 1)
;;         (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
;;         (global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;;         (define-key evil-visual-state-map (kbd "y") 'wsl-copy)
;;         (add-hook 'vterm-copy-mode-hook
;;                 (lambda ()

;;                         (if vterm-copy-mode
;;                                 (progn
;;                                 (set-background-color "Grey31")
;;                                 (message "copy mode enabled")
;;                                 )
;;                         (progn
;;                                 (set-background-color "#282828")
;;                                 (message "copy mode disabled")
;;                                 )
;;                         )
;;                         )
;;                 )

;;         (add-hook 'evil-insert-state-entry-hook
;;                 (lambda ()
;;                 (progn
;;                 (set-background-color "Grey31")
;;                 (message "entered insert mode")
;;                 )))

;;         (add-hook 'evil-insert-state-exit-hook
;;                 (lambda ()
;;                 (progn
;;                 (set-background-color "#282828")
;;                 (message "exited insert mode")
;;                 )))
;;   )
(use-package! typescript-mode
  :config(setq typescript-indent-level 2))

(use-package! web-mode
  :config(setq web-mode-code-indent-offset 2))

(use-package! undo-tree
  :config(setq web-mode-code-indent-offset 2))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))
(defun copy-python-breakpoint-to-kill-ring()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (concat "b " (file-truename buffer-file-name) ":" (replace-regexp-in-string "Line " "" (what-line))))))

(map! :v "$" #'evil-last-non-blank)
(map! :i "C-k" #'kill-line)
(map! :after evil-org
      :map evil-org-mode-map
      :i "C-k" #'kill-line)

(use-package! treemacs
  :config
  (treemacs-follow-mode 1)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(defun evil-scroll-down-fixed ()
  (interactive)
  (evil-scroll-down 4))
(defun evil-scroll-up-fixed ()
  (interactive)
  (evil-scroll-up 4))

(map! :nve "C-d" #'evil-scroll-down-fixed)
(map! :nve "C-u" #'evil-scroll-up-fixed)

(map! :nve "C-c b" #'copy-python-breakpoint-to-kill-ring)
(map! :nve "C-c B" #'remove-python-project-breakpoints)
(map! :nve "C-c t" #'copy-python-test-path)
(map! :i "C-c v" #'evil-paste-after)


(defun python-mode-enter ()
  (flycheck-select-checker 'python-flake8))

(add-hook 'python-mode-hook 'python-mode-enter)

(map! :leader
      (:prefix ("p" . "project")
       :desc "Remove project breakpoints"
       "m r" #'remove-python-project-breakpoints))

(map! :leader
      (:prefix ("p" . "project")
       :desc "Disable project breakpoints"
       "m d" #'disable-python-project-breakpoints))

(map! :leader
      (:prefix ("p" . "project")
       :desc "Enable project breakpoints"
       "m e" #'enable-python-project-breakpoints))

(defun remove-python-project-breakpoints ()
  (interactive)
  (shell-command (format "%s %s" "~/code/utilities/remove-project-breakpoints.sh" (string-trim-right (projectile-project-root) "/")))
  )

(defun disable-python-project-breakpoints ()
  (interactive)
  (shell-command (format "%s %s" "~/code/utilities/disable-project-breakpoints.sh" (string-trim-right (projectile-project-root) "/")))
  )

(defun enable-python-project-breakpoints ()
  (interactive)
  (shell-command (format "%s %s" "~/code/utilities/enable-project-breakpoints.sh" (string-trim-right (projectile-project-root) "/")))
  )


(use-package! python-pytest
  :config
  (defun copy-python-test-path ()
    (interactive)
    (kill-new (replace-regexp-in-string (projectile-project-root) "" (concat "pytest "(buffer-file-name) "::" (python-pytest--current-defun))))
    (message "Test path copied to kill ring")
    )
  )


(add-hook 'vterm-copy-mode-hook
          (lambda ()
            (if vterm-copy-mode
                (progn
                  (set-background-color "#282828")
                  )
              (progn
                (set-background-color "black")
                )
              )
            )
          )

(define-key! :keymaps +default-minibuffer-maps
  "C-k" #'kill-line)

(use-package! doom-themes
  :init (setq doom-theme 'gruvbox-dark-medium))

(custom-set-faces
 '(default ((t (:background "black")))))
