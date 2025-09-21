# 第3章：エディタ基礎編 - 使いやすさの向上

### 🎯 この章の目標
- 基本的な編集機能の強化
- バックアップとundo機能の設定
- フォント設定のみ（テーマは第8章で設定）

### 📝 実装内容

```org
* 第3章の設定：基本的な使いやすさ

** フォント設定
#+begin_src emacs-lisp
  ;; フォント設定
  (when (member "Monaspace Neon" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Monaspace Neon"
                        :height 130))
#+end_src

** 行番号とカーソル行のハイライト
#+begin_src emacs-lisp
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
#+end_src

** Editor補助機能

*** 括弧の自動補完
#+begin_src emacs-lisp
  (use-package smartparens
    :ensure t
    :hook (prog-mode . smartparens-mode)
    :init
    (require 'smartparens-config)  ; デフォルト設定を読み込み
    :config
    (smartparens-global-mode t))  ; グローバルに有効化
#+end_src

*** ace-window（ウィンドウ移動）
#+begin_src emacs-lisp
  (use-package ace-window
    :ensure t
    :bind ("M-o" . ace-window)
    :custom
    (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
#+end_src

*** helpful（詳細なヘルプ）
#+begin_src emacs-lisp
  (use-package helpful
    :ensure t
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)
    ("C-h F" . helpful-function)
    ("C-h C" . helpful-command))
#+end_src

*** expand-region（選択範囲の拡張）
#+begin_src emacs-lisp
  (use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))
#+end_src

*** editorconfig（プロジェクト設定）
#+begin_src emacs-lisp
  (use-package editorconfig
    :ensure t
    :diminish
    :config
    (editorconfig-mode 1))
#+end_src
```

### ✨ この章で得られたもの
- ✅ フォント設定
- ✅ 安全な自動バックアップ機能
- ✅ ウィンドウ間の高速移動（ace-window）
- ✅ 詳細なヘルプシステム（helpful）
- ✅ プロジェクト設定の自動適用（editorconfig）

### 💡 トラブルシューティング

#### org-modeのコードブロック内インデント問題
**症状**: org-modeのemacs-lispコードブロック内で改行すると、意図せず行がインデントされる

**解決方法**: config.orgの先頭付近に以下を追加
```elisp
(setq org-src-preserve-indentation t)
```

**回避策**: C-jで改行、またはC-c 'で専用バッファで編集

*org-mode 9.4.3以降の既知のバグによる問題です*

#### シンボリックリンクファイルの確認を無効化
**症状**: Git管理下のファイルへのシンボリックリンクを開くたびに確認ダイアログが表示される

**解決方法**: 設定ファイルに以下を追加
```elisp
(setq vc-follow-symlinks t)
```

*必要に応じて個人の設定に追加してください*

---

[← 前の章へ](03_package.md) | [目次に戻る](00_introduction.md) | [次の章へ →](05_evil.md)