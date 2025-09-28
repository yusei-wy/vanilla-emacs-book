# 第6章：開発環境編 - LSPとGit統合

### 🎯 この章の目標
- 言語サーバー（LSP）の設定
- Gitインテグレーション
- プロジェクト管理

### 📝 実装内容

```org
* 第6章の設定：開発環境

** エラーチェックとLSP設定

*** Eglot（組み込みLSPクライアント）
#+begin_src emacs-lisp
  ;; Emacs 29+では組み込みのEglotを使用
  (use-package eglot
    :ensure nil  ; 組み込みパッケージ
    :hook ((go-mode . eglot-ensure)
           (typescript-mode . eglot-ensure)
           (js-mode . eglot-ensure)
           (js2-mode . eglot-ensure)
           (rjsx-mode . eglot-ensure)
           (python-mode . eglot-ensure)
           (haskell-mode . eglot-ensure)
           (web-mode . eglot-ensure)
           (json-mode . eglot-ensure)
           (yaml-mode . eglot-ensure))
    :config
    ;; LSPサーバーの設定
    (setq eglot-autoshutdown t)  ; バッファを閉じたらLSPサーバーも終了
    (setq eglot-sync-connect 0)  ; 非同期接続で高速化

    ;; Format on Save（オプション）
    ;; (add-hook 'before-save-hook
    ;;           (lambda ()
    ;;             (when (eglot-managed-p)
    ;;               (eglot-format-buffer))))

    ;; LSP関連キーバインド
    (with-eval-after-load 'general
      (general-def
        :states '(normal visual)
        :prefix "SPC l"
        :keymaps 'eglot-mode-map
        "a" '(eglot-code-actions :which-key "code actions")
        "r" '(eglot-rename :which-key "rename")
        "f" '(eglot-format :which-key "format")
        "d" '(xref-find-definitions :which-key "find definitions")
        "R" '(xref-find-references :which-key "find references"))))

  ;; 必要なLSPサーバーのインストール手順:
  ;; Go: go install golang.org/x/tools/gopls@latest
  ;; TypeScript/JavaScript: npm install -g typescript typescript-language-server
  ;; Python: pip install python-lsp-server
  ;; Haskell: ghcup install hls
  ;; HTML/CSS/JSON: npm install -g vscode-langservers-extracted
  ;; YAML: npm install -g yaml-language-server
#+end_src

*** Flymake（組み込みエラーチェッカー）
#+begin_src emacs-lisp
  ;; Flymakeの基本設定（Eglotと自動連携）
  (use-package flymake
    :ensure nil  ; 組み込みパッケージ
    :hook (prog-mode . flymake-mode)
    :config
    (setq flymake-no-changes-timeout 0.5)  ; 変更後0.5秒でチェック
    (setq flymake-start-syntax-check-on-newline t)
    (setq flymake-merge-all-diagnostics t)

    ;; エラー表示の改善
    (setq flymake-fringe-indicator-position 'left-fringe)

    ;; エラーチェック関連キーバインド
    (with-eval-after-load 'general
      (general-def
        :states '(normal visual)
        :prefix "SPC e"
        "n" '(flymake-goto-next-error :which-key "next error")
        "p" '(flymake-goto-prev-error :which-key "prev error")
        "l" '(flymake-show-buffer-diagnostics :which-key "list errors")
        "L" '(flymake-show-project-diagnostics :which-key "project errors"))))
#+end_src

*** オプション：Flycheck（より多くの言語サポートが必要な場合）
#+begin_src emacs-lisp
  ;; LSPが利用できない言語や追加のチェッカーが必要な場合のみ有効化
  ;; コメントアウトを外して使用

  ;; (use-package flycheck
  ;;   :ensure t
  ;;   :init (global-flycheck-mode)
  ;;   :config
  ;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;;   (setq flycheck-display-errors-delay 0.3))

  ;; ;; EglotとFlycheckを連携させる場合
  ;; (use-package flycheck-eglot
  ;;   :ensure t
  ;;   :after (flycheck eglot)
  ;;   :config
  ;;   (global-flycheck-eglot-mode 1))
#+end_src

** Magit（Git統合）
#+begin_src emacs-lisp
  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status))
    :config
    ;; Git関連キーバインド
    (with-eval-after-load 'general
      (general-def
        :states '(normal visual)
        :prefix "SPC g"
        "g" '(magit-status :which-key "status")
        "b" '(magit-blame :which-key "blame")
        "l" '(magit-log :which-key "log"))))
#+end_src

** Treemacs（ファイルツリー）
#+begin_src emacs-lisp
  (use-package treemacs
    :ensure t
    :defer t
    :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'deferred)
    ;; Treemacs関連キーバインド
    (with-eval-after-load 'general
      (general-def
        :states '(normal visual)
        :prefix "SPC o"
        "p" '(treemacs :which-key "project tree"))))

  (use-package treemacs-evil
    :after (treemacs evil)
    :ensure t
    :defer t)
#+end_src

** diff-hl（変更箇所の可視化）
#+begin_src emacs-lisp
  (use-package diff-hl
    :ensure t
    :hook ((prog-mode . diff-hl-mode)
           (dired-mode . diff-hl-dired-mode))
    :config
    (diff-hl-flydiff-mode))
#+end_src
```

### ✨ この章で得られたもの
- ✅ リアルタイムエラーチェック（Flymake）
- ✅ LSPによる高度な言語機能（定義ジャンプ、リファクタリング等）
- ✅ 言語サーバーとの統合（Eglot）
- ✅ プロジェクト全体のエラー表示
- ✅ 賢いコード補完とエラー表示
- ✅ Gitの全機能をEmacsから操作
- ✅ VSCode風のファイルツリー
- ✅ 変更箇所のリアルタイム可視化（diff-hl）

---

[← 前の章へ](06_completion.md) | [目次に戻る](00_introduction.md) | [次の章へ →](08_terminal.md)