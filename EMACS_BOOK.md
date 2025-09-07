# 📖 My Emacs Journey: Doom脱却からVanilla Emacsマスターへの道

## はじめに

このドキュメントは、Doom Emacsから独自のVanilla Emacs環境を構築していく旅の記録です。
各章を進めるごとに、あなたのEmacsは少しずつ成長し、最終的には完全にカスタマイズされた、
世界でただ一つのエディタになります。

### この本の読み方

- 各章は独立しており、必要な章から始められます
- ただし、順番に進めることで最も効果的に学習できます
- 各章の最後には「この章で得られたもの」があり、達成感を味わえます
- トラブルシューティングは各章に含まれています

---

## 第0章：準備編 - macOSへのEmacsインストール

### 🎯 この章の目標
- macOSに最適なEmacsをインストールする
- 日本語環境を完璧にセットアップする
- 今後のカスタマイズの土台を作る

### 📝 実装内容

#### Emacs Mac Port（EMP版）のインストール

##### なぜEMP版か？
- **日本語入力**: IMEのちらつき問題が解決済み、`(mac-auto-ascii-mode 1)`で自動切り替え
- **macOS統合**: スムーズスクロール、トラックパッドジェスチャー、Retinaディスプレイ対応
- **高パフォーマンス**: Apple Silicon（M1/M2/M3）での最適化

##### インストールオプションの選択

```shell
# 推奨：フル機能版（プログラミング + 文書作成）
brew tap railwaycat/emacsmacport
brew install emacs-mac \
  --with-native-comp \
  --with-imagemagick \
  --with-librsvg \
  --with-starter

# 最小限版（プログラミング中心）
brew install emacs-mac --with-native-comp

# vterm（高性能ターミナル）を使う場合の追加パッケージ
brew install cmake libvterm
```

##### 各オプションの効果
| オプション | 効果 | 必要度 |
|-----------|------|--------|
| `--with-native-comp` | Elispを高速化（2-5倍） | ⭐⭐⭐ 必須 |
| `--with-imagemagick` | PDF/画像表示 | ⭐⭐⭐ 推奨 |
| `--with-librsvg` | SVGアイコン表示 | ⭐⭐ 推奨 |
| `--with-xwidgets` | 内蔵Webブラウザ | ⭐ オプション |
| `--with-starter` | CLIからGUI起動 | ⭐⭐ 便利 |

##### インストール後の設定

```shell
# GUIアプリとして配置
cp -a /opt/homebrew/opt/emacs-mac/Emacs.app /Applications

# 動作確認
open /Applications/Emacs.app
emacs --version  # ターミナルから確認
```

### ⚠️ 注意事項
- **初回起動**: `--with-native-comp`使用時は、初回起動でパッケージコンパイルのため時間がかかる（5-10分）
- **ディスク容量**: ネイティブコンパイルは約1GB追加で使用
- **依存パッケージ**: ほとんどは自動インストールされるが、vtermのみ手動で`cmake`と`libvterm`が必要

### ✨ この章で得られたもの
- ✅ 日本語入力が完璧に動作するEmacs
- ✅ macOSとの深い統合（スムーズスクロール、ジェスチャー）
- ✅ ネイティブコンパイルによる高速動作
- ✅ 画像・PDF表示機能
- ✅ これから育てていく「種」としてのEmacs

---

## 第1章：基礎編 - 高速起動と最小限のEmacs設定

### 🎯 この章の目標
- 起動時間1秒以内を実現する基盤構築
- org-modeによる文芸的プログラミング環境
- パフォーマンス測定の仕組み

### 📝 実装内容

#### Emacsアーキテクチャ図

```
┌──────────────────────────────────────────────────────────┐
│                    Emacs 起動フロー                       │
└──────────────────────────────────────────────────────────┘
                            │
                            ▼
                ┌───────────────────┐
                │  early-init.el    │ ← 最速実行
                │  ・GC無効化       │ 
                │  ・UI要素削除     │
                │  ・最小限の設定   │
                └───────────────────┘
                            │
                            ▼
                ┌───────────────────┐
                │    init.el        │ ← ブートストラップ
                │  ・GC復元         │
                │  ・org-tangle     │
                │  ・config.el読込  │
                └───────────────────┘
                            │
                            ▼
                ┌───────────────────┐
                │   config.org      │ ← 設定本体
                │  ・パッケージ管理 │
                │  ・エディタ設定   │
                │  ・開発環境       │
                └───────────────────┘
                            │
                            ▼
                    ⏱️ 起動完了！
              目標: < 1.0秒 🚀
```

#### ディレクトリ構造
```shell
mkdir -p ~/.emacs.d/{backups,auto-saves}
cd ~/.emacs.d
touch early-init.el init.el config.org
```

#### early-init.el（起動高速化の核心）
```elisp
;;; early-init.el --- 早期初期化で高速起動を実現 -*- lexical-binding: t -*-

;; Emacsバージョンチェック
(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher required" minver)))

;; 起動時間計測開始
(defconst emacs-start-time (current-time))

;; GC閾値を一時的に最大化（起動後に戻す）
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ファイルハンドラーを一時的に空に
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; UI要素の早期無効化
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; 不要な起動処理を無効化
(setq package-enable-at-startup nil
      site-run-file nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; ネイティブコンパイル警告を抑制
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

(provide 'early-init)
;;; early-init.el ends here
```

#### init.el（ブートストラップとパフォーマンス復元）
```elisp
;;; init.el --- Bootstrap configuration -*- lexical-binding: t -*-

;; 起動後にGC設定を実用的な値に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)
            (setq file-name-handler-alist default-file-name-handler-alist)
            ;; 起動時間を表示
            (let ((startup-time (float-time
                                (time-subtract after-init-time before-init-time))))
              (message "=====================================")
              (message " Emacs loaded in %.2f seconds" startup-time)
              (message " with %d garbage collections" gcs-done)
              (when (< startup-time 1.0)
                (message " 🚀 Excellent! Sub-second startup!"))
              (message "====================================="))))

;; config.orgのtangle/load
(let* ((org-file (expand-file-name "config.org" user-emacs-directory))
       (el-file (expand-file-name "config.el" user-emacs-directory)))
  (when (or (not (file-exists-p el-file))
            (file-newer-than-file-p org-file el-file))
    (require 'org)
    (org-babel-tangle-file org-file))
  (load el-file nil t))

(provide 'init)
;;; init.el ends here
```

#### config.org（基盤設定）
```org
#+TITLE: My Emacs Configuration
#+AUTHOR: y.kasa
#+PROPERTY: header-args:emacs-lisp :tangle yes :mkdirp yes

* 基盤設定

** macOS固有設定
#+begin_src emacs-lisp
  ;; macOSでのMetaキー設定（日本語ユーザー向け）
  (when (eq system-type 'darwin)
    ;; EMP版の場合
    (when (eq window-system 'mac)
      (setq mac-command-modifier 'meta))     ; Cmd = M-
    
    ;; GNU Emacs版の場合
    (when (eq window-system 'ns)
      (setq ns-command-modifier 'meta)))     ; Cmd = M-

  ;; NOTE: 特殊文字入力が必要な場合は以下の設定を追加
  ;; (setq mac-option-modifier 'meta)         ; 左Option = M-
  ;; (setq mac-right-option-modifier 'none)   ; 右Option = 特殊文字入力用
#+end_src

** エンコーディング
#+begin_src emacs-lisp
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
#+end_src

** バックアップ設定
#+begin_src emacs-lisp
  (use-package emacs
    :custom
    (make-backup-files t)
    (version-control t)
    (kept-new-versions 20)
    (kept-old-versions 5)
    (delete-old-versions t)
    (backup-directory-alist
     `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
    (auto-save-file-name-transforms
     `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
    (backup-by-copying t)
    (vc-make-backup-files t))
#+end_src

** パフォーマンス最適化
#+begin_src emacs-lisp
  ;; 大きなファイルの警告閾値
  (setq large-file-warning-threshold (* 100 1024 1024))  ; 100MB
  ;; 読み込みプロセスの最適化
  (setq read-process-output-max (* 1024 1024))  ; 1MB
#+end_src
```

### ✨ この章で得られたもの
- ✅ 高速起動を実現する最適化設定
- ✅ 起動時間の自動測定と表示
- ✅ org-modeによる設定管理基盤
- ✅ 日本語環境の完全サポート
- ✅ 安全なバックアップ体制

### 💡 Git管理のヒント

.emacs.dをGitで管理する場合の`.gitignore`設定例：

```gitignore
# 自動生成ファイル
config.el
*.elc
*~

# バックアップ・自動保存
backups/
auto-saves/

# パッケージ管理
elpaca/

# キャッシュ・履歴
eln-cache/
transient/
.cache/
recentf
savehist
undo-fu-session/

# LSP関連
.lsp-session-*
.dap-breakpoints

# プロジェクト関連
projectile-bookmarks.eld
projects

# OS固有ファイル
.DS_Store
```

**管理すべきファイル**：
- `init.el`
- `early-init.el`
- `config.org`
- カスタムスニペット（作成した場合）

これにより設定を安全にバージョン管理できます。

---

## 第2章：パッケージ管理編 - Elpacaによる拡張性の獲得

### 🎯 この章の目標
- 最新のパッケージマネージャーElpacaを導入
- use-packageによる宣言的な設定
- 最初のパッケージをインストールしてみる

### 📝 実装内容

config.orgに追加：

```org
* 第2章の設定：パッケージ管理

** Elpacaのブートストラップ
なぜElpaca？ → 非同期処理で高速、straight.elの後継

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

** use-package統合
#+begin_src emacs-lisp
  (elpaca elpaca-use-package
    (elpaca-use-package-mode))
#+end_src

** 最初のパッケージ：which-key
画面下部にキーバインドのヘルプを表示

#+begin_src emacs-lisp
  (use-package which-key
    :ensure t
    :init
    (which-key-mode)
    :custom
    (which-key-idle-delay 0.3))
#+end_src
```

### ✨ この章で得られたもの
- ✅ パッケージを自由にインストールできる環境
- ✅ キーバインドのヘルプ表示（which-key）
- ✅ 非同期処理による高速なパッケージ管理

---

## 第3章：エディタ基礎編 - 使いやすさの向上

### 🎯 この章の目標
- 基本的な編集機能の強化
- バックアップとundo機能の設定
- 見た目の改善（テーマ、フォント）

### 📝 実装内容

```org
* 第3章の設定：基本的な使いやすさ

** テーマ設定（見た目の改善）
#+begin_src emacs-lisp
  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-nord-aurora t))

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
- ✅ 美しいテーマとフォント設定
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

## 第4章：Evil編 - Vimの力を手に入れる

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
* 第4章の設定：Vimキーバインド

** Evil（Vimエミュレーション）
#+begin_src emacs-lisp
  (use-package evil
    :ensure t
    :init
    ;; evil-collection用の設定（順序重要）
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-i-jump nil)  ; TABキーの競合回避
    :config
    (evil-mode 1))
#+end_src

** 日本語入力（IME）の自動制御
#+begin_src emacs-lisp
  ;; macOS: Normal State移行時にIMEを自動的にオフ
  (when (eq system-type 'darwin)
    (use-package evil
      :hook (evil-normal-state-entry-hook . mac-change-language-to-us)))
#+end_src

** Evil拡張プラグイン（必須）
Vimの便利機能をEmacsでも使えるようにします。

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
    (evil-goggles-duration 0.100))

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

** リーダーキー（SPCキー）の設定
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

    ;; 基本的なキーバインド
    (leader-def
      "f" '(:ignore t :which-key "file")
      "f f" '(find-file :which-key "find file")
      "f s" '(save-buffer :which-key "save file")
      "b" '(:ignore t :which-key "buffer")
      "b b" '(switch-to-buffer :which-key "switch buffer")
      "b k" '(kill-this-buffer :which-key "kill buffer")
      ;; vundoのキーバインドもここに追加
      "u" '(vundo :which-key "undo tree visualizer")))
#+end_src

** Undo/Redo機能（より安定的な実装）
従来のundo-treeには履歴破損の問題があるため、より安定的な組み合わせを使用します。

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
    :config
    ;; Unicodeシンボルで美しく表示
    (setq vundo-glyph-alist vundo-unicode-symbols))

  ;; undo-fu-session: 履歴の永続化（オプション）
  (use-package undo-fu-session
    :ensure t
    :config
    (global-undo-fu-session-mode)
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

## 第5章：補完システム編 - 賢い入力支援

### 🎯 この章の目標
- 高速な補完システムの構築
- ファジー検索の導入
- コマンド実行の効率化

### 📝 実装内容

```org
* 第5章の設定：補完システム

** Vertico（縦型補完UI）
#+begin_src emacs-lisp
  (use-package vertico
    :ensure t
    :init
    (vertico-mode)
    :custom
    (vertico-cycle t))
#+end_src

** Orderless（柔軟な補完スタイル）
#+begin_src emacs-lisp
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))
#+end_src

** Marginalia（補完候補の追加情報）
#+begin_src emacs-lisp
  (use-package marginalia
    :ensure t
    :init
    (marginalia-mode))
#+end_src

** Consult（検索と移動の強化）
#+begin_src emacs-lisp
  (use-package consult
    :ensure t
    :bind (("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("M-y" . consult-yank-pop)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)))
#+end_src

** VSCode風操作の実現

*** コマンドパレット（M-x強化）
#+begin_src emacs-lisp
  ;; M-xをVSCodeのコマンドパレット風に
  (global-set-key (kbd "M-x") 'execute-extended-command)

  ;; リーダーキーでもアクセス可能に
  (with-eval-after-load 'general
    (leader-def
      "SPC" '(execute-extended-command :which-key "M-x")
      ":" '(eval-expression :which-key "eval")))
#+end_src

*** プロジェクトファイル検索（Cmd+P相当）
#+begin_src emacs-lisp
  ;; projectile: プロジェクト管理
  (use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :custom
    (projectile-completion-system 'default)
    (projectile-enable-caching t))

  ;; consult-projectile: consultとの統合
  (use-package consult-projectile
    :ensure t
    :after (consult projectile))

  ;; キーバインド（VSCode風）
  (with-eval-after-load 'general
    (leader-def
      "p" '(:ignore t :which-key "project")
      "p f" '(consult-projectile-find-file :which-key "find file")
      "p p" '(consult-projectile-switch-project :which-key "switch project")
      ;; Ctrl+P風のファイル検索
      "f p" '(consult-projectile-find-file :which-key "quick open")))
#+end_src

*** プロジェクト内テキスト検索・置換（Cmd+Shift+F相当）
#+begin_src emacs-lisp
  ;; ripgrepによる高速検索
  (use-package consult
    :ensure t
    :config
    ;; ripgrepの詳細設定
    (setopt consult-ripgrep-args
            "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

  ;; wgrep: 検索結果を直接編集して一括置換
  (use-package wgrep
    :ensure t
    :config
    (setopt wgrep-auto-save-buffer t)        ; 変更を自動保存
    (setopt wgrep-change-readonly-file t))   ; 読み取り専用ファイルも編集可能に

  ;; embark: アクション実行フレームワーク
  (use-package embark
    :ensure t
    :bind
    (("C-." . embark-act)
     ("C-;" . embark-dwim))
    :init
    ;; 検索結果でCを押すとoccurバッファを開く
    (setopt prefix-help-command #'embark-prefix-help-command))

  ;; embark-consult: embarkとconsultの統合
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  ;; キーバインド
  (with-eval-after-load 'general
    (leader-def
      "/" '(consult-ripgrep :which-key "search project")
      "s" '(:ignore t :which-key "search")
      "s p" '(consult-ripgrep :which-key "search project")
      "s l" '(consult-line :which-key "search lines")
      "s r" '(projectile-replace :which-key "replace in project")
      "s R" '(projectile-replace-regexp :which-key "replace regexp")))

  ;; 使い方：
  ;; 1. SPC / または SPC s p でプロジェクト内検索
  ;; 2. C-c C-o で検索結果をoccurバッファに送る（consultのデフォルト）
  ;; 3. e でwgrepモードに入る
  ;; 4. 通常のEmacsコマンドで編集（複数ファイルを同時編集）
  ;; 5. C-c C-c で変更を適用、C-c C-k でキャンセル
  ;; 6. C-x s で変更したファイルを保存
#+end_src

*** マルチカーソル（evil-mc）
#+begin_src emacs-lisp
  (use-package evil-mc
    :ensure t
    :after evil
    :config
    (global-evil-mc-mode 1))

  ;; VSCode風のキーバインド
  (with-eval-after-load 'general
    (general-define-key
     :states '(normal visual)
     "C-d" 'evil-mc-make-and-goto-next-match  ; Ctrl+D: 次の同じ単語を選択
     "C-S-d" 'evil-mc-skip-and-goto-next-match ; Ctrl+Shift+D: スキップして次へ
     "C-M-d" 'evil-mc-make-all-cursors))      ; Ctrl+Alt+D: すべて選択

  ;; リーダーキーでも操作可能
  (with-eval-after-load 'general
    (leader-def
      "m" '(:ignore t :which-key "multiple-cursors")
      "m a" '(evil-mc-make-all-cursors :which-key "select all")
      "m n" '(evil-mc-make-and-goto-next-match :which-key "next match")
      "m p" '(evil-mc-make-and-goto-prev-match :which-key "prev match")
      "m q" '(evil-mc-undo-all-cursors :which-key "quit")))
#+end_src

** Corfu（コード補完）
#+begin_src emacs-lisp
  (use-package corfu
    :ensure t
    :custom
    (corfu-auto t)
    (corfu-auto-delay 0.0)
    (corfu-auto-prefix 2)
    :init
    (global-corfu-mode))
#+end_src
```

### ✨ この章で得られたもの
- ✅ 高速でスマートな補完
- ✅ ファジー検索でファイルやコマンドを素早く見つける
- ✅ コード入力時の自動補完
- ✅ VSCodeライクなコマンドパレット（SPC SPC）
- ✅ プロジェクト内ファイル検索（Ctrl+P風）
- ✅ マルチカーソル編集（Ctrl+D）
- ✅ プロジェクト内テキスト検索・置換（Cmd+Shift+F相当）
- ✅ wgrepによる検索結果の直接編集

---

## 第6章：開発環境編 - LSPとGit統合

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
    
    ;; キーバインド
    (with-eval-after-load 'general
      (leader-def
        :keymaps 'eglot-mode-map
        "l" '(:ignore t :which-key "lsp")
        "l a" '(eglot-code-actions :which-key "code actions")
        "l r" '(eglot-rename :which-key "rename")
        "l f" '(eglot-format :which-key "format")
        "l d" '(xref-find-definitions :which-key "find definitions")
        "l R" '(xref-find-references :which-key "find references"))))
  
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
    
    ;; キーバインド
    (with-eval-after-load 'general
      (leader-def
        "e" '(:ignore t :which-key "errors")
        "e n" '(flymake-goto-next-error :which-key "next error")
        "e p" '(flymake-goto-prev-error :which-key "prev error")
        "e l" '(flymake-show-buffer-diagnostics :which-key "list errors")
        "e L" '(flymake-show-project-diagnostics :which-key "project errors"))))
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
    :bind (("C-x g" . magit-status)))

  (leader-def
    "g" '(:ignore t :which-key "git")
    "g g" '(magit-status :which-key "status")
    "g b" '(magit-blame :which-key "blame")
    "g l" '(magit-log :which-key "log"))
#+end_src

** Treemacs（ファイルツリー）
#+begin_src emacs-lisp
  (use-package treemacs
    :ensure t
    :defer t
    :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'deferred))

  (use-package treemacs-evil
    :after (treemacs evil)
    :ensure t)

  (leader-def
    "o p" '(treemacs :which-key "project tree"))
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

## 第7章：ターミナル統合編 - シェルとの完全融合

### 🎯 この章の目標
- 完全なターミナルエミュレータの統合
- Emacs内でのシェル作業の効率化
- コードとターミナルのシームレスな連携

### 📝 実装内容

```org
* 第7章の設定：ターミナル統合

** Vterm（完全ターミナルエミュレーション）
#+begin_src emacs-lisp
  (use-package vterm
    :ensure t
    :defer t
    :custom
    (vterm-max-scrollback 10000)       ; スクロールバック行数
    (vterm-shell "/bin/zsh")            ; 使用するシェル
    (vterm-kill-buffer-on-exit t)      ; シェル終了時にバッファを閉じる
    (vterm-copy-exclude-prompt t)      ; コピー時にプロンプトを除外
    :config
    ;; Vterm内でEvilモードの切り替え
    (evil-set-initial-state 'vterm-mode 'emacs))

  ;; multi-vterm：複数ターミナル管理
  (use-package multi-vterm
    :ensure t
    :defer t
    :config
    (setq multi-vterm-dedicated-window-height 30))

  ;; キーバインド
  (leader-def
    "o" '(:ignore t :which-key "open")
    "o t" '(multi-vterm :which-key "terminal")
    "o T" '(multi-vterm-dedicated-toggle :which-key "dedicated terminal")
    "o n" '(multi-vterm-next :which-key "next terminal")
    "o p" '(multi-vterm-prev :which-key "prev terminal"))
#+end_src

** ターミナルとの連携強化
#+begin_src emacs-lisp
  ;; vterm-toggle: 簡単なターミナル切り替え
  (use-package vterm-toggle
    :ensure t
    :defer t
    :custom
    (vterm-toggle-fullscreen-p nil)     ; フルスクリーンで開かない
    (vterm-toggle-scope 'project)       ; プロジェクトごとにターミナル
    :bind ("C-`" . vterm-toggle)        ; C-`でトグル
    :config
    ;; ポップアップウィンドウの設定
    (setq vterm-toggle-hide-method 'reset-window-configration))

  ;; コマンドをVtermで実行
  (defun my/run-in-vterm (command)
    "Run COMMAND in vterm."
    (interactive "sCommand: ")
    (let ((vterm-shell command))
      (vterm)))

  ;; ファイルパスをVtermに送る
  (defun my/send-to-vterm (text)
    "Send TEXT to vterm."
    (interactive "sText: ")
    (vterm-send-string text))
#+end_src

** ターミナル便利機能
#+begin_src emacs-lisp
  ;; 現在のディレクトリでVtermを開く
  (defun my/vterm-here ()
    "Open vterm in current directory."
    (interactive)
    (let ((default-directory (if (buffer-file-name)
                                 (file-name-directory (buffer-file-name))
                               default-directory)))
      (vterm)))

  ;; Vterm内でのコピー/ペースト改善
  (defun my/vterm-yank ()
    "Yank in vterm mode."
    (interactive)
    (if (evil-normal-state-p)
        (vterm-yank)
      (vterm-send-C-y)))

  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-y") 'my/vterm-yank))

  ;; リーダーキー追加
  (leader-def
    "o h" '(my/vterm-here :which-key "terminal here")
    "o r" '(my/run-in-vterm :which-key "run command"))
#+end_src
```

### 💡 使い方のヒント

- **C-`**: ターミナルのトグル（表示/非表示）
- **SPC o t**: 新しいターミナルを開く
- **SPC o T**: 専用ターミナルのトグル
- **SPC o h**: 現在のディレクトリでターミナルを開く
- **Vterm内でC-c C-t**: Evilモードへ切り替え（テキスト選択用）

### ✨ この章で得られたもの
- ✅ 完全なターミナルエミュレーション
- ✅ 複数ターミナルの管理機能
- ✅ プロジェクトごとのターミナル分離
- ✅ コードとターミナルのシームレスな切り替え
- ✅ Evilモードとの適切な統合

---

## 第8章：UI/UX改善編 - 美しさと使いやすさの追求

### 🎯 この章の目標
- モードラインのカスタマイズ
- アイコンの追加
- 全体的な見た目の統一

### 📝 実装内容

```org
* 第8章の設定：UI/UXの向上

** Doom Modeline（美しいモードライン）
#+begin_src emacs-lisp
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-height 25)
    (doom-modeline-bar-width 3)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon t))
#+end_src

** Nerd Icons（アイコン表示）
#+begin_src emacs-lisp
  (use-package nerd-icons
    :ensure t)

  (use-package treemacs-nerd-icons
    :ensure t
    :after treemacs
    :config
    (treemacs-load-theme "nerd-icons"))
#+end_src

** Rainbow Delimiters（括弧の色分け）
#+begin_src emacs-lisp
  (use-package rainbow-delimiters
    :ensure t
    :hook (prog-mode . rainbow-delimiters-mode))
#+end_src

** Doomテーマ（美しいテーマ集）
#+begin_src emacs-lisp
  (use-package doom-themes
    :ensure t
    :config
    ;; グローバル設定
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    ;; テーマをロード（doom-oneがおすすめ）
    (load-theme 'doom-one t)
    ;; org-modeの見た目を改善
    (doom-themes-org-config))
#+end_src

** Solaire Mode（バッファの視覚的差別化）
#+begin_src emacs-lisp
  (use-package solaire-mode
    :ensure t
    :config
    (solaire-global-mode +1))
#+end_src

** hl-todo（キーワードハイライト）
#+begin_src emacs-lisp
  (use-package hl-todo
    :ensure t
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-keyword-faces
          '(("TODO"   . "#FF0000")
            ("FIXME"  . "#FF0000")
            ("DEBUG"  . "#A020F0")
            ("NOTE"   . "#FF6E00")
            ("HACK"   . "#FF6E00"))))
#+end_src
```

### ✨ この章で得られたもの
- ✅ プロフェッショナルな見た目
- ✅ アイコンによる視認性の向上
- ✅ 括弧の対応が一目でわかる
- ✅ Doom Emacsの美しいテーマ（doom-themes）
- ✅ ファイルバッファとその他の視覚的差別化（solaire-mode）
- ✅ TODO/FIXMEの自動ハイライト（hl-todo）

---

## 第9章：言語別設定編 - 各言語への最適化

### 🎯 この章の目標
- 各プログラミング言語の専用モード設定
- 言語固有の便利機能とインデント設定
- 構造化された設定で将来の拡張に対応

### 📝 実装内容

```org
* 第9章の設定：言語別カスタマイズ

** プログラミング言語

*** Go言語
#+begin_src emacs-lisp
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :hook ((go-mode . (lambda ()
                        (setq tab-width 4)
                        (setq indent-tabs-mode t))))
    :config
    ;; gofmtの代わりにgoimportsを使用（自動import整理）
    (setq gofmt-command "goimports"))
#+end_src

*** Haskell
#+begin_src emacs-lisp
  (use-package haskell-mode
    :ensure t
    :mode "\\.hs\\'"
    :custom
    (haskell-indentation-layout-offset 4)
    (haskell-indentation-starter-offset 4)
    (haskell-indentation-left-offset 4))
#+end_src

*** TypeScript/JavaScript
#+begin_src emacs-lisp
  ;; TypeScript
  (use-package typescript-mode
    :ensure t
    :mode ("\\.ts\\'" "\\.tsx\\'")
    :custom
    (typescript-indent-level 2))

  ;; JavaScript（標準のjs-modeを拡張）
  (use-package js2-mode
    :ensure t
    :mode "\\.js\\'"
    :custom
    (js2-basic-offset 2)
    (js2-strict-missing-semi-warning nil))

  ;; JSX/React サポート
  (use-package rjsx-mode
    :ensure t
    :mode ("\\.jsx\\'" "components\\/.*\\.js\\'"))
#+end_src

** マークアップ・スタイルシート言語

*** Web開発（HTML/CSS）
#+begin_src emacs-lisp
  ;; web-mode: HTML, CSS, JSXなどを統合サポート
  (use-package web-mode
    :ensure t
    :mode ("\\.html\\'" "\\.css\\'" "\\.scss\\'" "\\.sass\\'")
    :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-enable-auto-pairing t)
    (web-mode-enable-css-colorization t)
    (web-mode-enable-current-element-highlight t))

  ;; Emmet: HTML/CSSの高速コーディング
  (use-package emmet-mode
    :ensure t
    :hook ((web-mode . emmet-mode)
           (html-mode . emmet-mode)
           (css-mode . emmet-mode))
    :config
    (setq emmet-expand-jsx-className? t))  ; React対応
#+end_src

** データ形式

*** JSON
#+begin_src emacs-lisp
  (use-package json-mode
    :ensure t
    :mode ("\\.json\\'" "\\.jsonc\\'")
    :hook (json-mode . (lambda ()
                         (setq-local js-indent-level 2))))
#+end_src

*** YAML
#+begin_src emacs-lisp
  (use-package yaml-mode
    :ensure t
    :mode ("\\.yml\\'" "\\.yaml\\'")
    :hook (yaml-mode . (lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
#+end_src

** 共通設定

*** フォーマッター統合
#+begin_src emacs-lisp
  ;; format-all: 多言語対応の統一フォーマッター
  (use-package format-all
    :ensure t
    :commands format-all-mode
    :hook (prog-mode . format-all-mode)
    :config
    ;; 保存時に自動フォーマット（オプション）
    (setq-default format-all-formatters
                  '(("TypeScript" prettier)
                    ("JavaScript" prettier)
                    ("JSON" prettier)
                    ("HTML" prettier)
                    ("CSS" prettier)
                    ("YAML" prettier)
                    ("Go" goimports)
                    ("Haskell" ormolu))))
#+end_src

*** Tree-sitter（構文ハイライト強化）
#+begin_src emacs-lisp
  ;; Emacs 29+の場合、tree-sitterモードを有効化
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
    (setq treesit-language-source-alist
          '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
            (json "https://github.com/tree-sitter/tree-sitter-json" "master" "src")
            (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
            (html "https://github.com/tree-sitter/tree-sitter-html" "master" "src")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src")
            (go "https://github.com/tree-sitter/tree-sitter-go" "master" "src")
            (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src"))))
#+end_src
```

### ✨ この章で得られたもの
- ✅ Go, Haskell, TypeScript/JavaScript, HTML/CSS, JSON, YAMLの完全サポート
- ✅ 各言語に最適化されたインデント設定
- ✅ web-modeによる統合的なWeb開発環境
- ✅ Emmetによる高速HTML/CSSコーディング
- ✅ format-allによる統一的なコードフォーマット
- ✅ Tree-sitterによる高精度な構文ハイライト（Emacs 29+）
- ✅ 構造化された設定で新しい言語の追加が容易

---

## 第10章：パフォーマンス最適化編 - 究極の高速化

### 🎯 この章の目標
- 起動時間の徹底的な高速化（0.3秒を目指す）
- メモリ使用量の最適化とプロファイリング
- 遅延読み込みの完全マスター
- パフォーマンス計測と継続的改善

### 📝 実装内容

#### パフォーマンス改善グラフ

```
起動時間の最適化結果
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

初期状態    ████████████████████████████████ 3.2秒
            
Step 1      ████████████████████ 2.0秒  
GC最適化    ▼ 37.5% 削減

Step 2      ████████████ 1.2秒
遅延読込    ▼ 40% 削減

Step 3      ██████ 0.6秒
Nativeｺﾝﾊﾟｲﾙ ▼ 50% 削減

最終結果    ███ 0.3秒 🚀
            ▼ 90.6% 削減！

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

#### メモリ使用量の推移

```
メモリ使用量 (MB)
    ↑
200 │     ╭─╮
    │    ╱   ╲    GCMHによる
150 │   ╱     ╲   スマートGC
    │  ╱       ╲
100 │ ╱         ╲_______________
    │╱                安定稼働
 50 │
    │
  0 └────────────────────────────→
    起動  1分   5分   10分   時間

通常のEmacs: 常に高いメモリ使用量
GCMH使用時: アイドル時に自動的にGC実行
```

```org
* 第10章の設定：パフォーマンス最適化

** GCMH（Garbage Collector Magic Hack）
#+begin_src emacs-lisp
  ;; GCMHで賢いガベージコレクション管理
  (use-package gcmh
    :ensure t
    :diminish
    :init
    ;; 初期値を高めに設定
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold (* 16 1024 1024)  ; 16MB
          gcmh-verbose nil)
    :config
    (gcmh-mode 1))
#+end_src

** 遅延読み込み戦略
#+begin_src emacs-lisp
  ;; use-packageのグローバル設定
  (setq use-package-always-defer t
        use-package-always-ensure t
        use-package-expand-minimally t
        use-package-compute-statistics t)  ; 統計情報を収集

  ;; 重いパッケージの遅延読み込み例
  (use-package magit
    :defer t
    :commands (magit-status magit-clone)
    :bind ("C-x g" . magit-status))

  (use-package treemacs
    :defer t
    :commands treemacs)

  (use-package lsp-mode
    :defer t
    :commands (lsp lsp-deferred))

  ;; アイドル時に読み込むパッケージ
  (use-package yasnippet
    :defer 5  ; 5秒後に読み込み
    :config
    (yas-global-mode 1))
#+end_src

** プロファイリングツール
#+begin_src emacs-lisp
  ;; esupによる起動時間の詳細分析
  (use-package esup
    :ensure t
    :defer t
    :commands esup)

  ;; ベンチマーク関数
  (defun my/benchmark-init ()
    "Benchmark Emacs startup."
    (interactive)
    (message "Benchmarking...")
    (esup))

  ;; use-package統計表示
  (defun my/show-package-stats ()
    "Show use-package statistics."
    (interactive)
    (use-package-report))

  ;; メモリ使用量表示
  (defun my/memory-usage ()
    "Show current memory usage."
    (interactive)
    (message "Memory usage: %.2f MB"
             (/ (float (memory-info)) 1024 1024)))
#+end_src

** ファイルの遅延読み込み
#+begin_src emacs-lisp
  ;; 大きなファイルの警告閾値
  (setq large-file-warning-threshold (* 100 1024 1024))  ; 100MB

  ;; so-longモードで長い行のファイルを最適化
  (global-so-long-mode 1)

  ;; ファイルローカル変数の処理を高速化
  (setq enable-local-variables :safe)
#+end_src

** プロセスとネットワークの最適化
#+begin_src emacs-lisp
  ;; プロセス出力の読み込みバッファサイズ
  (setq read-process-output-max (* 3 1024 1024))  ; 3MB

  ;; LSPの最適化
  (with-eval-after-load 'lsp-mode
    (setq lsp-idle-delay 0.5
          lsp-log-io nil
          lsp-completion-provider :none
          lsp-prefer-flymake nil))
#+end_src

** フォントキャッシュの最適化
#+begin_src emacs-lisp
  ;; フォントキャッシュの圧縮を無効化
  (setq inhibit-compacting-font-caches t)

  ;; Unicode文字のフォント設定を最適化
  (set-fontset-font t 'unicode
                     (font-spec :family "Noto Sans CJK JP")
                     nil 'prepend)
#+end_src

** 起動時間測定の高度化
#+begin_src emacs-lisp
  ;; 詳細な起動時間レポート
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let* ((startup-time (float-time
                                   (time-subtract after-init-time
                                                 before-init-time)))
                     (gc-time (float-time
                              (time-subtract gc-elapsed
                                           (seconds-to-time 0)))))
                (message "=====================================")
                (message " Emacs loaded in %.3f seconds" startup-time)
                (message " with %d garbage collections taking %.3f seconds"
                        gcs-done gc-time)
                (message " %.1f%% of startup time was spent in GC"
                        (* 100 (/ gc-time startup-time)))
                (when (< startup-time 0.5)
                  (message " 🚀 Blazing fast startup achieved!"))
                (message "====================================="))))

  ;; パッケージ読み込み時間の追跡
  (defun my/time-package-loads ()
    "Time how long each package takes to load."
    (interactive)
    (require 'benchmark)
    (message "Package load times:")
    (dolist (feature features)
      (when (string-match "^[^/]+$" (symbol-name feature))
        (let ((time (benchmark-elapse (require feature))))
          (when (> time 0.01)
            (message "  %s: %.3fs" feature time))))))
#+end_src

** カスタムコマンド
#+begin_src emacs-lisp
  (leader-def
    "h" '(:ignore t :which-key "help/benchmark")
    "h b" '(my/benchmark-init :which-key "benchmark startup")
    "h p" '(my/show-package-stats :which-key "package stats")
    "h m" '(my/memory-usage :which-key "memory usage")
    "h t" '(my/time-package-loads :which-key "time packages"))
#+end_src
```

### ✨ この章で得られたもの
- ✅ 起動時間0.5秒以内の超高速Emacs
- ✅ GCMHによる賢いメモリ管理
- ✅ 詳細なパフォーマンス計測ツール
- ✅ use-package統計による最適化の可視化
- ✅ 大規模ファイルでも快適な動作
- ✅ LSPやプロセスの最適化設定
- ✅ 継続的なパフォーマンス改善の基盤

---

## 第11章：Org-mode活用編 - 最強のドキュメントシステム

### 🎯 この章の目標
- Org-modeの基本から応用まで習得
- GTDタスク管理システムの構築
- 文芸的プログラミングの実践
- ナレッジベースの構築

### 📝 実装内容

#### Org-modeワークフロー図

```
┌─────────────────────────────────────────────────────────┐
│               Org-mode GTDワークフロー                   │
└─────────────────────────────────────────────────────────┘

        ┌──────────┐
        │  Capture │ ← C-c c (どこからでも)
        │ 収集箱   │
        └──────────┘
             │
             ▼
        ┌──────────┐
        │  Inbox   │ ← 未整理タスク
        │  整理前  │
        └──────────┘
             │
      ┌──────┴──────┐
      ▼             ▼
 ┌──────────┐  ┌──────────┐
 │ Projects │  │ Schedule │
 │ プロジェクト│  │ 予定表  │
 └──────────┘  └──────────┘
      │             │
      └──────┬──────┘
             ▼
        ┌──────────┐
        │  Agenda  │ ← C-c a (統合ビュー)
        │ 実行計画 │
        └──────────┘
             │
        ┌────┴────┐
        ▼         ▼
    ┌──────┐  ┌──────┐
    │ DOING│  │ DONE │
    │ 実行中│  │ 完了 │
    └──────┘  └──────┘
             │
             ▼
        ┌──────────┐
        │ Archive  │ ← 定期的にアーカイブ
        │  保管庫  │
        └──────────┘
```

#### タスクのライフサイクル

```
TODO → DOING → DONE
  ↓      ↓      ↓
WAIT  CANCELED  Archive

状態遷移キー:
• t: TODO
• d: DOING  
• w: WAIT (理由記録)
• x: DONE (時刻記録)
• c: CANCELED (理由記録)
```

```org
* 第11章の設定：Org-mode活用

** 基本設定とキーバインド
#+begin_src emacs-lisp
  (use-package org
    :ensure nil  ; 組み込みパッケージ
    :defer t
    :custom
    ;; 基本設定
    (org-startup-indented t)
    (org-hide-emphasis-markers t)
    (org-pretty-entities t)
    (org-ellipsis " ▼")
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-src-preserve-indentation t)
    
    ;; TODOキーワード
    (org-todo-keywords
     '((sequence "TODO(t)" "DOING(d)" "WAIT(w@)" "|" "DONE(x)" "CANCELED(c@)")))
    
    ;; TODOキーワードの色
    (org-todo-keyword-faces
     '(("TODO" . (:foreground "#ff6c6b" :weight bold))
       ("DOING" . (:foreground "#98be65" :weight bold))
       ("WAIT" . (:foreground "#ECBE7B" :weight bold))
       ("DONE" . (:foreground "#50a14f" :weight bold))
       ("CANCELED" . (:foreground "#ff6c6b" :strike-through t))))
    
    :config
    ;; コードブロックの実行確認を無効化
    (setq org-confirm-babel-evaluate nil)
    
    ;; 実行可能な言語
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t))))

  ;; キーバインド
  (leader-def
    "n" '(:ignore t :which-key "notes")
    "n n" '(org-capture :which-key "capture")
    "n a" '(org-agenda :which-key "agenda")
    "n l" '(org-store-link :which-key "store link"))
#+end_src

** Org-agenda（GTDタスク管理）
#+begin_src emacs-lisp
  (use-package org-agenda
    :ensure nil
    :after org
    :custom
    (org-agenda-files '("~/org/"))
    (org-agenda-window-setup 'current-window)
    (org-agenda-span 'day)
    (org-agenda-start-with-log-mode t)
    (org-log-done 'time)
    (org-log-into-drawer t)
    
    ;; カスタムビュー
    (org-agenda-custom-commands
     '(("d" "Dashboard"
        ((agenda "" ((org-deadline-warning-days 7)))
         (todo "DOING"
               ((org-agenda-overriding-header "実行中のタスク")))
         (todo "TODO"
               ((org-agenda-overriding-header "TODOリスト")))
         (todo "WAIT"
               ((org-agenda-overriding-header "待機中"))))))))
#+end_src

** Org-capture（素早いメモ）
#+begin_src emacs-lisp
  (use-package org-capture
    :ensure nil
    :after org
    :custom
    (org-capture-templates
     '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Inbox")
        "* TODO %?\n  %i\n  %a")
       ("n" "Note" entry (file+datetree "~/org/notes.org")
        "* %?\nEntered on %U\n  %i\n  %a")
       ("j" "Journal" entry (file+datetree "~/org/journal.org")
        "* %?\n%U\n")
       ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
        "* MEETING with %? :meeting:\n%T")
       ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
        "* %?\n%T"))))
#+end_src

** Org-roam（ナレッジベース）
#+begin_src emacs-lisp
  (use-package org-roam
    :ensure t
    :defer t
    :custom
    (org-roam-directory "~/org-roam/")
    (org-roam-db-location "~/org-roam/org-roam.db")
    (org-roam-completion-everywhere t)
    
    :config
    (org-roam-db-autosync-mode)
    
    ;; デイリーノート設定
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?"
             :target (file+datetree "%<%Y-%m-%d>.org"
                                   :tree-type week))))
    
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n t" . org-roam-tag-add)
           ("C-c n a" . org-roam-alias-add)
           ("C-c n l" . org-roam-buffer-toggle)
           ("C-c n d" . org-roam-dailies-capture-today)))

  ;; org-roam-ui: ナレッジグラフの可視化
  (use-package org-roam-ui
    :ensure t
    :after org-roam
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t))
#+end_src

** Org-modern（モダンな見た目）
#+begin_src emacs-lisp
  (use-package org-modern
    :ensure t
    :after org
    :hook (org-mode . org-modern-mode)
    :custom
    (org-modern-star '("◉" "○" "◈" "◇" "✳"))
    (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
    (org-modern-checkbox
     '((?X . "☑") (?- . "☐") (?\s . "☐")))
    (org-modern-table-vertical 1)
    (org-modern-table-horizontal 0.2))
#+end_src

** Org-download（画像の貼り付け）
#+begin_src emacs-lisp
  (use-package org-download
    :ensure t
    :after org
    :config
    (setq org-download-method 'directory)
    (setq org-download-image-dir "./images")
    (setq org-download-heading-lvl nil)
    (setq org-download-timestamp "%Y%m%d-%H%M%S_")
    
    :bind
    (:map org-mode-map
          ("C-c i" . org-download-clipboard)))
#+end_src

** Org-present（プレゼンテーション）
#+begin_src emacs-lisp
  (use-package org-present
    :ensure t
    :defer t
    :config
    (add-hook 'org-present-mode-hook
              (lambda ()
                (org-present-big)
                (org-display-inline-images)
                (org-present-hide-cursor)
                (org-present-read-only)))
    (add-hook 'org-present-mode-quit-hook
              (lambda ()
                (org-present-small)
                (org-remove-inline-images)
                (org-present-show-cursor)
                (org-present-read-write))))
#+end_src

** エクスポート設定
#+begin_src emacs-lisp
  ;; HTMLエクスポートの美化
  (use-package htmlize
    :ensure t
    :defer t)

  ;; Markdownエクスポート
  (use-package ox-md
    :ensure nil
    :after org)

  ;; PDFエクスポート（LaTeX経由）
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("japanese-article"
                   "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{xeCJK}
\\setCJKmainfont{Noto Sans CJK JP}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
#+end_src
```

### 💡 使い方のヒント

#### GTDワークフロー
1. `SPC n n` で素早くタスクをキャプチャ
2. `SPC n a` でアジェンダビューを開く
3. `d` でダッシュボードビューを表示
4. タスクを整理して優先順位をつける

#### ナレッジベース構築
1. `C-c n f` でノートを検索・作成
2. `C-c n i` で他のノートへのリンクを挿入
3. `C-c n l` でバックリンクを表示
4. `M-x org-roam-ui-open` でグラフを可視化

#### 文芸的プログラミング
1. `#+begin_src` でコードブロックを作成
2. `C-c C-c` でコードを実行
3. `C-c '` でコードブロックを別バッファで編集
4. `C-c C-e` でエクスポート

### ✨ この章で得られたもの
- ✅ GTDベースのタスク管理システム
- ✅ Org-captureによる素早いメモ取り
- ✅ Org-roamによるZettelkasten風ナレッジベース
- ✅ 文芸的プログラミングの実践環境
- ✅ プレゼンテーション作成機能
- ✅ 美しいドキュメントのエクスポート
- ✅ 画像の簡単な貼り付けと管理
- ✅ モダンで見やすいOrg文書

---

## 第12章：デバッグとテスト編 - 品質の追求

### 🎯 この章の目標
- 効率的なデバッグ環境の構築
- テスト駆動開発（TDD）の実践
- REPLを活用した対話的開発
- デバッガーの完全活用

### 📝 実装内容

#### TDDサイクル図

```
┌──────────────────────────────────────────────────────────┐
│                    TDDサイクル                            │
└──────────────────────────────────────────────────────────┘

           ┌─────────────┐
           │   1. RED    │
           │ テスト失敗  │ ← テストを先に書く
           └─────────────┘
                  │
                  ▼
           ┌─────────────┐
           │  2. GREEN   │
           │ テスト成功  │ ← 最小限の実装
           └─────────────┘
                  │
                  ▼
           ┌─────────────┐
           │ 3. REFACTOR │
           │ リファクタ  │ ← コード改善
           └─────────────┘
                  │
                  └────────┐
                          ▼
                    [繰り返し]

実行時間の目安:
• RED → GREEN: 5-10分
• GREEN → REFACTOR: 10-15分
• 1サイクル: 15-30分
```

#### デバッグワークフロー

```
問題発生
    │
    ▼
┌────────────┐     ブレークポイント設定
│ DAP-mode   │ ←─────────────────────┐
│ デバッガー │                        │
└────────────┘                        │
    │                                 │
    ▼                                 │
┌────────────┐     変数確認          │
│ ステップ   │ → [locals][watch]     │
│   実行     │                        │
└────────────┘                        │
    │                                 │
    ▼                                 │
┌────────────┐                        │
│ REPL評価   │ → 対話的に検証        │
└────────────┘                        │
    │                                 │
    ▼                                 │
問題特定 ─────────────────────────────┘
    │
    ▼
修正 & テスト追加
```

```org
* 第12章の設定：デバッグとテスト

** DAP-mode（デバッグアダプタープロトコル）
#+begin_src emacs-lisp
  (use-package dap-mode
    :ensure t
    :defer t
    :custom
    (dap-auto-configure-features '(sessions locals controls tooltip))
    :config
    ;; デバッグUIの設定
    (dap-ui-mode 1)
    (dap-tooltip-mode 1)
    (tooltip-mode 1)
    (dap-ui-controls-mode 1)
    
    ;; 言語別の設定
    (require 'dap-python)
    (require 'dap-node)
    (require 'dap-go)
    
    ;; Python
    (setq dap-python-debugger 'debugpy)
    
    ;; Node.js
    (dap-node-setup)
    
    ;; Go
    (require 'dap-dlv-go))

  ;; キーバインド
  (leader-def
    "d" '(:ignore t :which-key "debug")
    "d d" '(dap-debug :which-key "start debug")
    "d n" '(dap-next :which-key "next")
    "d i" '(dap-step-in :which-key "step in")
    "d o" '(dap-step-out :which-key "step out")
    "d c" '(dap-continue :which-key "continue")
    "d r" '(dap-restart-frame :which-key "restart")
    "d q" '(dap-quit :which-key "quit")
    "d b" '(dap-breakpoint-toggle :which-key "toggle breakpoint")
    "d B" '(dap-ui-breakpoints :which-key "list breakpoints")
    "d e" '(dap-eval :which-key "eval")
    "d E" '(dap-eval-thing-at-point :which-key "eval at point")
    "d l" '(dap-ui-locals :which-key "locals")
    "d s" '(dap-ui-sessions :which-key "sessions"))
#+end_src

** テストフレームワーク統合
#+begin_src emacs-lisp
  ;; Buttercup（Emacs Lisp用テスト）
  (use-package buttercup
    :ensure t
    :defer t)

  ;; ERT（組み込みテストフレームワーク）
  (use-package ert
    :ensure nil
    :defer t
    :config
    (defun my/run-ert-tests ()
      "Run all ERT tests in current buffer."
      (interactive)
      (ert-run-tests-interactively t)))

  ;; pytest（Python）
  (use-package python-pytest
    :ensure t
    :after python
    :bind (:map python-mode-map
                ("C-c t" . python-pytest-dispatch)))

  ;; jest（JavaScript/TypeScript）
  (use-package jest
    :ensure t
    :after (js2-mode typescript-mode)
    :hook ((js2-mode typescript-mode) . jest-minor-mode))

  ;; go-test（Go）
  (use-package gotest
    :ensure t
    :after go-mode
    :bind (:map go-mode-map
                ("C-c t f" . go-test-current-file)
                ("C-c t t" . go-test-current-test)
                ("C-c t p" . go-test-current-project)
                ("C-c t b" . go-test-current-benchmark)
                ("C-c t c" . go-test-current-coverage)))
#+end_src

** REPL統合
#+begin_src emacs-lisp
  ;; IELM（Emacs Lisp REPL）
  (use-package ielm
    :ensure nil
    :defer t
    :config
    (defun my/ielm-send-line-or-region ()
      "Send the current line or region to IELM."
      (interactive)
      (let ((text (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'line t))))
        (with-current-buffer "*ielm*"
          (goto-char (point-max))
          (insert text)
          (ielm-send-input)))))

  ;; Python REPL
  (use-package python
    :ensure nil
    :defer t
    :config
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i"))

  ;; Node.js REPL
  (use-package nodejs-repl
    :ensure t
    :defer t
    :bind (:map js2-mode-map
                ("C-c C-e" . nodejs-repl-send-last-expression)
                ("C-c C-r" . nodejs-repl-send-region)
                ("C-c C-b" . nodejs-repl-send-buffer)
                ("C-c C-z" . nodejs-repl-switch-to-repl)))

  ;; inf-ruby（Ruby REPL）
  (use-package inf-ruby
    :ensure t
    :defer t
    :hook (ruby-mode . inf-ruby-minor-mode))

  ;; Cider（Clojure REPL）
  (use-package cider
    :ensure t
    :defer t)
#+end_src

** Edebug（Emacs Lispデバッガー）
#+begin_src emacs-lisp
  ;; Edebug設定
  (use-package edebug
    :ensure nil
    :defer t
    :custom
    (edebug-print-length 50)
    (edebug-print-level 10)
    (edebug-trace t)
    :config
    ;; カスタムキーバインド
    (define-key edebug-mode-map (kbd "n") 'edebug-next-mode)
    (define-key edebug-mode-map (kbd "c") 'edebug-continue-mode)
    (define-key edebug-mode-map (kbd "b") 'edebug-set-breakpoint)
    (define-key edebug-mode-map (kbd "u") 'edebug-unset-breakpoint)
    (define-key edebug-mode-map (kbd "e") 'edebug-eval-expression)
    (define-key edebug-mode-map (kbd "q") 'top-level))

  ;; デバッグ用ヘルパー関数
  (defun my/edebug-defun ()
    "Instrument function for debugging."
    (interactive)
    (eval-defun 'edebugit))
#+end_src

** ベンチマークとプロファイリング
#+begin_src emacs-lisp
  ;; benchmark
  (use-package benchmark-init
    :ensure t
    :config
    ;; To disable collection after init
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; プロファイラー関数
  (defun my/profile-this (func &rest args)
    "Profile FUNC with ARGS."
    (elp-instrument-function func)
    (apply func args)
    (elp-results)
    (elp-restore-function func))

  ;; メモリプロファイラー
  (defun my/memory-profile ()
    "Show memory usage by package."
    (interactive)
    (require 'memory-usage)
    (memory-usage))
#+end_src

** エラーハンドリング強化
#+begin_src emacs-lisp
  ;; better-errors
  (use-package better-jumper
    :ensure t
    :config
    (better-jumper-mode +1))

  ;; エラー時のスタックトレース改善
  (setq debug-on-error nil)  ; 本番環境ではnil
  (setq debug-on-quit nil)

  ;; カスタムエラーハンドラー
  (defun my/toggle-debug-on-error ()
    "Toggle debug-on-error."
    (interactive)
    (setq debug-on-error (not debug-on-error))
    (message "Debug on error: %s" debug-on-error))

  ;; エラーログの改善
  (use-package log4e
    :ensure t
    :defer t)
#+end_src

** テスト駆動開発支援
#+begin_src emacs-lisp
  ;; TDDワークフロー
  (defun my/tdd-cycle ()
    "Run TDD cycle: test -> code -> refactor."
    (interactive)
    (let ((mode major-mode))
      (cond
       ((eq mode 'python-mode) (python-pytest-dispatch))
       ((eq mode 'js2-mode) (jest))
       ((eq mode 'go-mode) (go-test-current-test))
       ((eq mode 'emacs-lisp-mode) (ert t))
       (t (message "No test runner for %s" mode)))))

  ;; ウォッチモード
  (use-package filewatcher
    :ensure t
    :defer t
    :config
    (defun my/auto-test-on-save ()
      "Automatically run tests on save."
      (when (and (bound-and-true-p my/auto-test-mode)
                 (not (string-match "test" (buffer-file-name))))
        (my/tdd-cycle))))

  ;; 自動テストモード
  (define-minor-mode my/auto-test-mode
    "Automatically run tests on save."
    :lighter " AutoTest"
    :global nil
    (if my/auto-test-mode
        (add-hook 'after-save-hook 'my/auto-test-on-save nil t)
      (remove-hook 'after-save-hook 'my/auto-test-on-save t)))
#+end_src

** カバレッジ表示
#+begin_src emacs-lisp
  ;; Coverage overlay
  (use-package coverlay
    :ensure t
    :defer t
    :config
    (setq coverlay:tested-line-background-color "#2F4F4F"))

  ;; Python coverage
  (use-package pycoverage
    :ensure t
    :after python
    :config
    (defun my/python-coverage ()
      "Show Python test coverage."
      (interactive)
      (pycoverage-mode 1)
      (pycoverage-refresh)))
#+end_src
```

### 💡 使い方のヒント

#### デバッグワークフロー
1. `SPC d b` でブレークポイントを設定
2. `SPC d d` でデバッグセッション開始
3. `SPC d n/i/o` でステップ実行
4. `SPC d e` で式を評価
5. `SPC d l` でローカル変数を確認

#### TDDサイクル
1. テストを書く（失敗することを確認）
2. 最小限のコードで実装
3. テストが通ることを確認
4. リファクタリング
5. `M-x my/auto-test-mode` で自動テスト有効化

#### REPL活用
1. 言語に応じたREPLを起動
2. コードを選択して `C-c C-r` で送信
3. 対話的に結果を確認
4. `C-c C-z` でREPLバッファに切り替え

### ✨ この章で得られたもの
- ✅ VSCode風のビジュアルデバッグ環境
- ✅ 多言語対応のデバッグアダプター
- ✅ 各言語のテストフレームワーク統合
- ✅ TDDワークフローの自動化
- ✅ REPL駆動開発の環境
- ✅ コードカバレッジの可視化
- ✅ パフォーマンスプロファイリング
- ✅ 高度なエラーハンドリング

---

## エピローグ：これからの旅

おめでとうございます！あなたは自分だけのEmacsを作り上げました。

### 🎓 学んだこと
- org-modeによる文芸的プログラミング
- Emacsの内部構造への理解
- パッケージ管理とカスタマイズの技術

### 🚀 次のステップ
- より高度なカスタマイズ
- 独自のパッケージ作成
- コミュニティへの貢献

### 📚 参考資料
- [Emacs公式マニュアル](https://www.gnu.org/software/emacs/manual/)
- [Elpaca公式ドキュメント](https://github.com/progfolio/elpaca)
- [Evil Guide](https://github.com/noctuid/evil-guide)

---

## 付録A：トラブルシューティング

### よくある問題と解決法

#### パッケージがインストールできない
```elisp
M-x elpaca-manager  ; パッケージの状態を確認
M-x elpaca-rebuild  ; パッケージの再ビルド
```

#### 起動が遅い
```elisp
M-x profiler-start RET RET
; Emacsを再起動
M-x profiler-report
```

#### 文字化けする
```elisp
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
```

---

## 付録B：config.orgの完全版構成

最終的なconfig.orgは以下の構成になります：

1. 基礎設定（第1章）
2. パッケージ管理（第2章）
3. エディタ基礎（第3章）
4. Evil設定（第4章）
5. 補完システム（第5章）
6. 開発環境（第6章）
7. ターミナル（第7章）
8. UI/UX（第8章）
9. 言語別設定（第9章）
10. 最終調整（第10章）

各章の設定は独立しており、必要に応じて有効/無効を切り替えられます。

---

## 付録C：設定のモジュール化（参考）

設定が大きくなってきたら、機能別にファイルを分けることで管理しやすくなります。
これは一つの方法であり、正解ではありません。自分に合った構造を見つけてください。

### Doom Emacs風のモジュール構造

```
~/.emacs.d/
├── early-init.el      # 起動高速化
├── init.el            # ブートストラップ
├── config.org         # メイン設定とモジュール読み込み
└── modules/           
    ├── completion.org # 補完システム全般
    ├── editor.org     # Evil、編集補助機能
    ├── emacs.org      # Emacs基本設定
    ├── lang.org       # 全言語設定
    ├── tools.org      # 開発ツール（LSP、Git、Terminal等）
    └── ui.org         # テーマ、モードライン、見た目
```

### モジュールの内容

| モジュール | 含まれる機能 | 対応する章 |
|-----------|-------------|-----------|
| **emacs.org** | 文字コード、バックアップ、基本設定 | 第1-3章の基礎部分 |
| **editor.org** | Evil、which-key、ace-window等 | 第3-4章 |
| **completion.org** | Corfu、Cape、Consult、Orderless等 | 第5章 |
| **tools.org** | LSP、Git、Projectile、Treemacs、Vterm | 第6-7章 |
| **lang.org** | Go、Haskell、TypeScript、Web開発等 | 第9章 |
| **ui.org** | doom-themes、doom-modeline、solaire-mode等 | 第8章 |

### config.orgでのモジュール読み込み例

```elisp
;; モジュール読み込み
(org-babel-load-file (expand-file-name "modules/emacs.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "modules/editor.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "modules/completion.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "modules/ui.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "modules/tools.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "modules/lang.org" user-emacs-directory))
```

### 移行のタイミング

以下のような場合にモジュール化を検討：

- config.orgが1000行を超えた
- 特定の機能を探すのに時間がかかる
- 設定の一部を無効化してデバッグしたい
- 複数のマシンで異なる設定を使いたい

### その他の分割方法

1. **機能別により細かく分割**
   - 各言語を個別ファイルに（go.org、python.org等）
   - ツールも個別に（git.org、lsp.org、terminal.org）

2. **用途別に分割**
   - personal.org（個人設定）
   - work.org（仕事用設定）
   - experimental.org（実験的な設定）

3. **優先度別に分割**
   - essential.org（必須設定）
   - optional.org（オプション設定）
   - experimental.org（試験的設定）

### 注意点

- 分割しすぎると管理が複雑になる
- 最初は単一ファイルで始める
- 必要に応じて段階的に分割
- 自分の使い方に合った構造を選ぶ

---

*付録D〜F、索引については別ファイル `APPENDIX.md` を参照してください。*

*Happy Hacking! 🎉*
