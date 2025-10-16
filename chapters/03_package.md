[← 前の章へ](02_foundation.md) | [目次に戻る](00_introduction.md) | [次の章へ →](04_editor.md)

---

# 第2章：パッケージ管理編 - Elpacaによる拡張性の獲得

### 🎯 この章の目標
- 最新のパッケージマネージャーElpacaを導入
- use-packageによる宣言的な設定
- 最初のパッケージをインストールしてみる

### 📝 実装内容

config.orgに追加：

```org
* core

** packages

*** elpaca bootstrap

#+begin_src emacs-lisp
  (defvar elpaca-installer-version 0.11)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                :ref nil :depth 1 :inherit ignore
                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                :build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
        (build (expand-file-name "elpaca/" elpaca-builds-directory))
        (order (cdr elpaca-order))
        (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (<= emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                    ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                    ,@(when-let* ((depth (plist-get order :depth)))
                                                        (list (format "--depth=%d" depth) "--no-single-branch"))
                                                    ,(plist-get order :repo) ,repo))))
                    ((zerop (call-process "git" nil buffer t "checkout"
                                          (or (plist-get order :ref) "--"))))
                    (emacs (concat invocation-directory invocation-name))
                    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                    ((require 'elpaca))
                    ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))
#+end_src

*** use-package binding
#+begin_src emacs-lisp
  (elpaca elpaca-use-package
    (elpaca-use-package-mode))
#+end_src

*** which-key

#+begin_src emacs-lisp
  (use-package which-key
    :ensure t
    :init
    (which-key-mode)
    :custom
    (which-key-idle-delay 0.3))
#+end_src

*** exec-path-from-shell: シェルの環境変数を Emacs に取り込む

*なぜ必要か*:
- mise でインストールした LSP などを利用できる様に

#+begin_src emacs-lisp
  (use-package exec-path-from-shell
    :ensure t
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))
#+end_src

*** performance optimization

**** gcmh: スマートGC管理
アイドル時に自動的にGCを実行し、作業中のパフォーマンスを維持。

#+begin_src emacs-lisp
  (use-package gcmh
    :ensure t
    :diminish
    :init
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold (* 16 1024 1024)  ; 16MB
          gcmh-verbose nil)
    :config
    (gcmh-mode 1))
#+end_src

**** benchmark-init: 起動時間の詳細分析
各パッケージの読み込み時間を可視化。

#+begin_src emacs-lisp
  (use-package benchmark-init
    :ensure t
    :config
    (add-hook 'after-init-hook 'benchmark-init/deactivate))
#+end_src

**** esup: 詳細プロファイラー
M-x esup で起動プロセスの詳細分析。

#+begin_src emacs-lisp
  (use-package esup
    :ensure t
    :defer t
    :commands esup)
#+end_src
```

### ✨ この章で得られたもの
- ✅ パッケージを自由にインストールできる環境
- ✅ キーバインドのヘルプ表示（which-key）
- ✅ 非同期処理による高速なパッケージ管理
- ✅ シェル環境変数の自動取り込み（exec-path-from-shell）
- ✅ スマートGC管理（gcmh）
- ✅ 起動時間の詳細分析（benchmark-init）
- ✅ 詳細プロファイラー（esup）

---

[← 前の章へ](02_foundation.md) | [目次に戻る](00_introduction.md) | [次の章へ →](04_editor.md)