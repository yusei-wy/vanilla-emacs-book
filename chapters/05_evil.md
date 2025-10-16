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
    (evil-mode 1)
    ;; Eglot連携: g i で実装へジャンプ
    (with-eval-after-load 'eglot
      (evil-define-key 'normal 'global
        (kbd "g i") 'eglot-find-implementation)))
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
    (evil-collection-init)
    :custom
    ;; Dired でも SPC を leader key としてコマンドを呼び出せる様に
    (evil-collection-key-blacklist '("SPC")))

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

*** :after と :commands の使い分け

**** :after の使い方

*意味*: 指定したパッケージがロードされた後に、このパッケージをロード

*使うべき場合*:
- パッケージが他のパッケージに機能的に依存している
- 依存先がロードされないとエラーになる
- 組み込みパッケージ (~:ensure nil~) で general を使う場合

*例*:
#+begin_src emacs-lisp :tangle no
;; evil-collection は evil に機能的に依存
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; 組み込みパッケージで general を使う場合
(use-package eglot
  :ensure nil
  :after general
  :config
  (with-eval-after-load 'general
    (leader-def ...)))
#+end_src

**** :commands の使い方

*意味*: 指定したコマンドが実行された時に、パッケージを自動ロード

*使うべき場合*:
- パッケージが独立している
- キーバインドで使うコマンドが明確
- 起動時にロードする必要がない（起動時間改善）

*例*:
#+begin_src emacs-lisp :tangle no
;; たまにしか使わないパッケージ
(use-package vundo
  :ensure t
  :commands (vundo)
  :init
  (with-eval-after-load 'general
    (leader-def "u" '(vundo :which-key "undo tree"))))
#+end_src

**** 外部パッケージで general を使う場合

*重要*: ~:after general~ は不要（~with-eval-after-load 'general~ で十分）

*推奨パターン*:
#+begin_src emacs-lisp :tangle no
(use-package パッケージ名
  :ensure t
  :commands (コマンド1 コマンド2)  ; 自動ロード
  :init
  (with-eval-after-load 'general
    (leader-def "キー" '(コマンド1 :which-key "..."))))
#+end_src

**** 組み込みパッケージで general を使う場合

*注意*: ~:after general~ が必要、かつ ~:config~ セクションを使う

*理由*: 組み込みパッケージの ~:init~ は general の前に実行される可能性がある

*正しい例*:
#+begin_src emacs-lisp :tangle no
(use-package flymake
  :ensure nil
  :after general
  :config  ; :init ではなく :config
  (with-eval-after-load 'general
    (leader-def
      "e n" '(flymake-goto-next-error :which-key "next error")
      ...)))
#+end_src

*該当パッケージ*: eglot, flymake

**** キーバインド定義の使い分け

*1. leader-def*: SPC プレフィックス付きコマンド用（VSCode 風）
- 例: ~(leader-def "/" '(consult-ripgrep :which-key "search"))~

*2. general-def*: 特定モードや状態の直接バインド用
- Evil state や keymap を明示的に指定する場合
- 例: ~(general-def :states '(insert) "C-n" #'company-select-next)~

*3. with-eval-after-load*: 遅延読み込みパッケージ対応
- ~:init~ セクション内で、依存パッケージ読み込み後にキーバインドを定義
- 例: ~(with-eval-after-load 'general (leader-def ...))~

*推奨パターン*:
- use-package 内では ~:init~ + ~with-eval-after-load 'general~ を使用
- SPC コマンドは ~leader-def~ を優先使用
- モード固有のキーは ~general-def~ を使用

#+begin_src emacs-lisp
  (use-package general
    :ensure t
    :after evil
    :demand t
    :config
    (general-def
      :states '(normal visual motion)
      "SPC" nil)

    (general-create-definer leader-def
      :states '(normal visual)
      :prefix "SPC")

    (leader-def
      ;; グループ定義
      "f" '(:ignore t :which-key "file")
      "b" '(:ignore t :which-key "buffer")
      "w" '(:ignore t :which-key "window")
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
      "t" '(:ignore t :which-key "test")
      ;; コア機能（常に必要）
      "SPC" '(execute-extended-command :which-key "M-x")
      ":" '(eval-expression :which-key "eval")
      ;; 基本ファイル操作
      "f f" '(find-file :which-key "find file")
      "f s" '(save-buffer :which-key "save file")
      ;; 基本バッファ操作
      "b b" '(switch-to-buffer :which-key "switch buffer")
      "b k" '(kill-this-buffer :which-key "kill buffer")
      ;; Window 操作
      "w d" '(delete-window :which-key "delete window")
      "w o" '(delete-other-windows :which-key "delete other windows")
      "w v" '(split-window-right :which-key "split right")
      "w s" '(split-window-below :which-key "split below")
      "w w" '(other-window :which-key "other window")))
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

** performance measurement

パフォーマンス測定用のヘルパー関数とキーバインド。

*なぜ elpaca-after-init-hook を使うのか*:
- 通常の ~after-init-hook~ はElpacaパッケージのロード完了前に実行される
- ~elpaca-after-init-hook~ を使うことで、全パッケージロード後にキーバインドを安全に登録できる

#+begin_src emacs-lisp
  (use-package emacs
    :ensure nil
    :commands (my/show-startup-time)
    :init
    (defun my/show-startup-time ()
      "Show Emacs startup time."
      (interactive)
      (message "Emacs loaded in %.2f seconds with %d garbage collections"
               (float-time (time-subtract after-init-time before-init-time))
               gcs-done))

    (add-hook 'elpaca-after-init-hook
      (lambda ()
        (when (fboundp 'leader-def)
          (leader-def
            "h b" '(my/show-startup-time :which-key "show startup time")
            "h B" '(benchmark-init/show-durations-tree :which-key "benchmark details")
            "h E" '(esup :which-key "profile startup")
            "h p" '(use-package-report :which-key "package stats"))))))
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