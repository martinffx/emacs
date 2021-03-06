;;; package --- Summary
;;; Commentary:


;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar my-packages '(better-defaults paredit idle-highlight-mode helm
                                      company-tern git-gutter expand-region
                                      js2-mode flycheck flycheck-pos-tip
                                      editorconfig scala-mode2 yasnippet
                                      ensime docker dockerfile-mode emacs-eclim))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;
;; START GLOBAL ;;
;;;;;;;;;;;;;;;;;;

;; startup screen
(setq inhibit-splash-screen t)

;; yasnippet
(yas-global-mode 1)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)

;; editorconfig
(editorconfig-mode 1)

;; flycheck
(global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; docker
(docker-global-mode 1)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; replace region when you paste
(delete-selection-mode 1)

;; whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;
;; END GLOBAL   ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; START JS     ;;
;;;;;;;;;;;;;;;;;;

;; js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; json
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

;;;;;;;;;;;;;;;;;;
;;   END JS     ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; START JAVA   ;;
;;;;;;;;;;;;;;;;;;

;; eclim
(defun ome-eclim-setup ()
  ;; (add-to-list 'eclim-eclipse-dirs "/opt/eclipse")
  (setq eclim-auto-save t
        eclim-executable (or (executable-find "eclim") "/opt/eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/opt/eclipse/eclimd")
        eclimd-wait-for-process nil
        eclimd-default-workspace "~/workspace/"
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)

  ;; Call the help framework with the settings above & activate
  ;; eclim-mode
  (help-at-pt-set-timer)

  ;; keep consistent which other auto-complete backend.
  ;;(custom-set-faces
  ;;   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
  ;;'(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face)))))

  ;; Hook eclim up with auto complete mode
  ;;(require 'ac-emacs-eclim-source)
  ;; (ac-emacs-eclim-config)


  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (global-company-mode t)
  
  (require 'eclimd)

  (add-hook 'java-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-emacs-eclim)
              (eclim-mode t))))

(when (executable-find "eclipse")
  (ome-install 'eclim))

;;;;;;;;;;;;;;;;;;
;; END JAVA     ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; START SCALA  ;;
;;;;;;;;;;;;;;;;;;

;; ensime
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook #'yas-minor-mode)
(with-eval-after-load 'company
  (define-key company-active-map [tab] nil))

;; sbt-mode
(add-hook 'sbt-mode-hook '(lambda ()
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate)))

;;;;;;;;;;;;;;;;;;
;;   END SCALA  ;;
;;;;;;;;;;;;;;;;;;


;;; init.el ends here
