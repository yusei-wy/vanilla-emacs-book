[← 前の章へ](04_editor.md) | [目次に戻る](00_introduction.md) | [次の章へ →](06_completion.md)

---

# 第4章：Evil編 - Vimの力を手に入れる

### 🎯 この章の目標
- Vimキーバインドの導入
- リーダーキーによる効率的な操作
- 安定したundo/redo機能と視覚的な編集履歴

### 📝 実装内容

#### Evil-modeステート遷移図

```
┌─────────────────────────────────────────────────────────────┐
│                  Evil-mode ステート遷移                      │
└─────────────────────────────────────────────────────────────┘

        ┌──────────────┐
        │ NORMAL Mode  │ ← デフォルト（コマンド実行）
        │   (ESC)      │
        └──────────────┘
         ↑ │    │    │ ↓
         │ i,I  v,V  : │
         │ a,A  C-v  Q │
         │ o,O       R │
         │ │    │    │ │
    ┌────────┐ ┌────────┐ ┌────────┐
    │ INSERT │ │ VISUAL │ │REPLACE │
    │  Mode  │ │  Mode  │ │  Mode  │
    └────────┘ └────────┘ └────────┘
        │         │ ↓        │
        └─────────┘ c,d,y    │
                  │ x,s      │
                  └──────────┘

    キー説明:
    • i,I,a,A,o,O: Insert modeへ
    • v,V,C-v: Visual modeへ（文字/行/矩形選択）
    • R: Replace modeへ
    • ESC: Normal modeへ戻る
    • :: Ex commandへ
```

#### Leader Keyマップ構造

```
          ┌─────────────┐
          │  SPC (Leader) │
          └─────────────┘
                 │
    ┌────────────┼────────────┐
    ▼            ▼            ▼
┌───────┐   ┌───────┐   ┌───────┐
│   f   │   │   b   │   │   g   │
│ file  │   │buffer │   │  git  │
└───────┘   └───────┘   └───────┘
    │           │           │
  f: find    b: switch   s: status
  s: save    k: kill     b: blame
  r: recent  d: delete   d: diff
```

```org
* editor

** evil
#+begin_src emacs-lisp
  (use-package evil
    :ensure t
    :init
    ;; evil-collection用の設定（順序重要）
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-i-jump nil)  ; TABキーの競合回避
    (setq evil-want-C-u-scroll t)  ; C-uでスクロールアップを有効化
    :config
    (evil-mode 1))
#+end_src

*** IME の自動制御
#+begin_src emacs-lisp
  (when (eq system-type 'darwin)
    (use-package evil
      :hook (evil-normal-state-entry-hook . mac-change-language-to-us)))
#+end_src

*** evil extensions

#+begin_src emacs-lisp
  ;; evil-collection: Evil未対応モードにVimキーバインド追加
  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

  ;; evil-surround: 囲み文字の操作（cs"' ds" ysiw"）
  (use-package evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode 1))

  ;; evil-commentary: コメント操作（gcc gc）
  (use-package evil-commentary
    :ensure t
    :after evil
    :config
    (evil-commentary-mode))

  ;; evil-snipe: 2文字検索（s/S）- オプション
  ;; 注意：sキー（substitute）を上書きします
  (use-package evil-snipe
    :ensure t
    :after evil
    :diminish
    :config
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1))

  ;; evil-indent-plus: インデントベースのテキストオブジェクト
  (use-package evil-indent-plus
    :ensure t
    :after evil
    :config
    (evil-indent-plus-default-bindings))

  ;; evil-goggles: 操作の視覚的フィードバック
  (use-package evil-goggles
    :ensure t
    :after evil
    :config
    (evil-goggles-mode)
    :custom
    (evil-goggles-duration 0.1))

  ;; evil-escape: jkでNormalモードへ
  (use-package evil-escape
    :ensure t
    :after evil
    :config
    (evil-escape-mode 1)
    :custom
    (evil-escape-key-sequence "jk")
    (evil-escape-delay 0.2))
#+end_src

** leader key (SPC)
#+begin_src emacs-lisp
  (use-package general
    :ensure t
    :after evil
    :config
    ;; SPCキーの既存バインドを解除
    (general-def
      :states '(normal visual motion)
      "SPC" nil)

    ;; リーダーキーの定義
    (general-create-definer leader-def
      :states '(normal visual)
      :prefix "SPC")

    ;; 基本的なキーバインド構造とコア機能
    (leader-def
      ;; グループ定義
      "f" '(:ignore t :which-key "file")
      "b" '(:ignore t :which-key "buffer")
      "p" '(:ignore t :which-key "project")
      "g" '(:ignore t :which-key "git")
      "l" '(:ignore t :which-key "lsp")
      "e" '(:ignore t :which-key "errors")
      "s" '(:ignore t :which-key "search")
      "m" '(:ignore t :which-key "multiple-cursors")
      "n" '(:ignore t :which-key "notes")
      "o" '(:ignore t :which-key "open")
      "h" '(:ignore t :which-key "help/benchmark")
      "d" '(:ignore t :which-key "debug")
      ;; コア機能（常に必要）
      "SPC" '(execute-extended-command :which-key "M-x")
      ":" '(eval-expression :which-key "eval")
      ;; 基本ファイル操作
      "f f" '(find-file :which-key "find file")
      "f s" '(save-buffer :which-key "save file")
      ;; 基本バッファ操作
      "b b" '(switch-to-buffer :which-key "switch buffer")
      "b k" '(kill-this-buffer :which-key "kill buffer")))
#+end_src

** undo

#+begin_src emacs-lisp
  ;; undo-fu: シンプルで安定したundo/redo
  (use-package undo-fu
    :ensure t
    :after evil
    :config
    ;; Evilとの統合
    (evil-set-undo-system 'undo-fu))

  ;; vundo: 必要な時だけ使うツリービジュアライザー
  (use-package vundo
    :ensure t
    :defer t
    :custom
    ;; デフォルトの3では履歴ツリーが見切れるため増やす
    (vundo-window-max-height 10)
    :init
    ;; vundo関連キーバインド
    (with-eval-after-load 'general
      (leader-def
        "u" '(vundo :which-key "undo tree visualizer")))
    :config
    ;; Unicodeシンボルで美しく表示
    (setq vundo-glyph-alist vundo-unicode-symbols))

  ;; undo-fu-session: 履歴の永続化（オプション）
  (use-package undo-fu-session
    :ensure t
    :defer t
    :hook (after-init . global-undo-fu-session-mode)
    :custom
    ;; Gitコミットメッセージなどは永続化しない
    (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
#+end_src
```

### ✨ この章で得られたもの
- ✅ Vimの効率的な編集操作
- ✅ Normal State移行時のIME自動オフ
- ✅ 安定したundo/redoと履歴表示
- ✅ 囲み文字操作やコメント操作
- ✅ Evil操作の視覚的フィードバック
- ✅ SPCキーによる直感的なコマンド実行

---

[← 前の章へ](04_editor.md) | [目次に戻る](00_introduction.md) | [次の章へ →](06_completion.md)