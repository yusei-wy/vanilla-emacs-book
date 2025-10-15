[← 前の章へ](06_completion.md) | [目次に戻る](00_introduction.md) | [次の章へ →](08_terminal.md)

---

# 第6章：開発環境編 - LSPとGit統合

### 🎯 この章の目標
- 言語サーバー（LSP）の設定
- Gitインテグレーション
- プロジェクト管理

### 📝 実装内容

```org
* lang

** lsp

*** eglot
#+begin_src emacs-lisp
  ;; Emacs 29+では組み込みのEglotを使用
  (use-package eglot
    :ensure nil  ; 組み込みパッケージ
    :after general
    :hook ((go-mode . eglot-ensure)
           (go-ts-mode . eglot-ensure)
           (typescript-mode . eglot-ensure)
           (typescript-ts-mode . eglot-ensure)
           (tsx-ts-mode . eglot-ensure)
           (js-mode . eglot-ensure)
           (js-ts-mode . eglot-ensure)
           (js2-mode . eglot-ensure)
           (rjsx-mode . eglot-ensure)
           (python-mode . eglot-ensure)
           (haskell-mode . eglot-ensure)
           (web-mode . eglot-ensure)
           (html-mode . eglot-ensure)
           (css-mode . eglot-ensure)
           (json-mode . eglot-ensure)
           (yaml-mode . eglot-ensure)
           (conf-toml-mode . eglot-ensure))
    :config
    (setq eglot-autoshutdown t)
    (setq eglot-sync-connect 0)

    ;; gopls (Go)
    (add-to-list 'eglot-server-programs
                 '((go-mode go-ts-mode) . ("gopls")))

    ;; Biome LSP (JS, TS, JSON)
    (add-to-list 'eglot-server-programs
                 '((js-mode js-ts-mode) . ("biome" "lsp-proxy")))
    (add-to-list 'eglot-server-programs
                 '((typescript-mode typescript-ts-mode tsx-ts-mode) . ("biome" "lsp-proxy")))
    (add-to-list 'eglot-server-programs
                 '(json-mode . ("biome" "lsp-proxy")))

    ;; vscode-langservers-extracted (HTML, CSS)
    (add-to-list 'eglot-server-programs
                 '(html-mode . ("vscode-html-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs
                 '(css-mode . ("vscode-css-language-server" "--stdio")))

    ;; YAML Language Server
    (add-to-list 'eglot-server-programs
                 '(yaml-mode . ("yaml-language-server" "--stdio")))

    ;; Taplo LSP (TOML)
    (add-to-list 'eglot-server-programs
                 '(conf-toml-mode . ("taplo" "lsp" "stdio")))

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
        "i" '(eglot-find-implementation :which-key "find implementation")
        "D" '(eglot-find-declaration :which-key "find declaration")
        "t" '(eglot-find-typeDefinition :which-key "find type definition")
        "R" '(xref-find-references :which-key "find references"))))

  ;; Format on save
  (add-hook 'before-save-hook
            (lambda ()
              (when (eglot-managed-p)
                (eglot-format-buffer))))

  ;; 必要なLSPサーバーのインストール手順:
  ;; Go: go install golang.org/x/tools/gopls@latest
  ;; TypeScript/JavaScript: npm install -g typescript typescript-language-server
  ;; Python: pip install python-lsp-server
  ;; Haskell: ghcup install hls
  ;; HTML/CSS/JSON: npm install -g vscode-langservers-extracted
  ;; YAML: npm install -g yaml-language-server
#+end_src

** checkers

*** flymake
#+begin_src emacs-lisp
  ;; Flymakeの基本設定（Eglotと自動連携）
  (use-package flymake
    :ensure nil  ; 組み込みパッケージ
    :after general
    :hook (prog-mode . flymake-mode)
    :config
    (setq flymake-no-changes-timeout 0.5)  ; 変更後0.5秒でチェック
    (setq flymake-start-syntax-check-on-newline t)
    (setq flymake-merge-all-diagnostics t)

    ;; エラー表示の改善
    (setq flymake-fringe-indicator-position 'left-fringe)

    ;; エラーチェック関連キーバインド
    (with-eval-after-load 'general
      (leader-def
        "e n" '(flymake-goto-next-error :which-key "next error")
        "e p" '(flymake-goto-prev-error :which-key "prev error")
        "e l" '(flymake-show-buffer-diagnostics :which-key "list errors")
        "e L" '(flymake-show-project-diagnostics :which-key "project errors"))))
#+end_src

* tools

** magit
#+begin_src emacs-lisp
  (use-package magit
    :ensure t
    :commands (magit-status magit-blame magit-log)
    :bind (("C-x g" . magit-status))
    :init
    ;; Git関連キーバインド
    (with-eval-after-load 'general
      (leader-def
        "g g" '(magit-status :which-key "status")
        "g b" '(magit-blame :which-key "blame")
        "g l" '(magit-log :which-key "log"))))
#+end_src

** treemacs
#+begin_src emacs-lisp
  (use-package treemacs
    :ensure t
    :commands (treemacs)
    :init
    ;; Treemacs関連キーバインド
    (with-eval-after-load 'general
      (leader-def
        "o p" '(treemacs :which-key "project tree")))
    :config
    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'deferred))

  (use-package treemacs-evil
    :after (treemacs evil)
    :ensure t
    :defer t)

  (use-package treemacs-nerd-icons
    :ensure t
    :after (treemacs nerd-icons)
    :config
    (treemacs-load-theme "nerd-icons"))
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