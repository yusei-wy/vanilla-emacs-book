# Doom Emacs → Vanilla Emacs 爆速移行計画

## 0. macOS用Emacsのインストール

### 推奨：Emacs Mac Port（EMP版）

#### なぜEMP版か
- **日本語入力の完璧なサポート**：IMEのちらつき問題が解決済み
- **macOS統合**：スムーズスクロール、トラックパッドジェスチャー対応
- **高パフォーマンス**：C-gサポート改善、効率的なイベント処理
- **Apple Silicon対応**：M1/M2/M3チップで安定動作

#### インストール手順

```bash
# 1. 依存パッケージのインストール
brew install git cmake libvterm libtool

# 2. Emacs Mac Portのインストール
brew tap railwaycat/emacsmacport
brew install emacs-mac

# 3. Applicationsフォルダに配置
cp -a /opt/homebrew/opt/emacs-mac/Emacs.app /Applications

# 4. 動作確認
emacs --version
open /Applications/Emacs.app
```

#### 他の選択肢との比較

| ビルド | 日本語入力 | macOS統合 | 推奨度 |
|--------|------------|-----------|--------|
| **EMP版** | ◎ 完璧 | ◎ 最高 | ★★★★★ |
| emacs-plus | △ 問題あり | ○ 良好 | ★★☆☆☆ |
| NS版（標準） | ○ 使える | △ 最小限 | ★★★☆☆ |

#### トラブルシューティング
- **vtermコンパイルエラー**：`brew reinstall cmake libvterm`でアーキテクチャを統一
- **モジュールサポート確認**：`emacs --batch --eval "(message \"%s\" system-configuration-features)" | grep MODULES`
- **再インストール時**：`brew uninstall emacs-mac`後に`brew install emacs-mac`（`brew reinstall`は避ける）

## 1. 目的

Doom Emacsから、起動速度1秒以内の軽量かつVSCodeライクなVanilla Emacs環境への移行を実現する。

## 2. なぜ移行するのか（背景・動機）

### 現状の課題
- **起動が遅い**: Doom Emacsは多機能だが重い
- **ブラックボックス化**: 何が動いているか把握しづらい
- **カスタマイズの限界**: Doom独自の抽象化により、細かい制御が困難

### 移行で得られるメリット
- **爆速起動**: 必要最小限の機能で0.5秒以内を目指す
- **完全な制御**: すべての設定を自分で管理
- **最新のベストプラクティス**: Elpaca等の最新ツールを活用
- **学習効果**: Emacsの仕組みを深く理解

## 3. 技術選定の根拠

| カテゴリ | 選択 | 理由 |
|---------|------|------|
| **パッケージ管理** | Elpaca + use-package | • straight.el作者による後継<br>• 非同期並列処理で最速<br>• use-packageは標準化済み<br>• setup.elも検討したが、標準性と情報量でuse-packageを選択 |
| **LSP** | Eglot | • Emacs29標準搭載<br>• 軽量でシンプル<br>• 設定不要で即動作 |
| **補完** | Corfu/Vertico | • 現在のDoomでも使用中<br>• 最速クラスの性能<br>• 作者が同じで統合性高い |
| **ファイル管理** | Treemacs | • VSCodeライクなサイドバー<br>• Git統合可能<br>• 視覚的にわかりやすい |
| **ターミナル** | Vterm | • 最高性能<br>• 完全なターミナルエミュレーション |
| **フォーマット** | Eglot (LSP) | • 言語サーバー経由で統一<br>• 設定がシンプル<br>• 各言語に最適なフォーマッタ |
| **バックアップ** | 番号付きバックアップ + backup-walker | • VSCode Timeline相当<br>• 履歴閲覧UI<br>• Git管理外で安全 |

## 4. ディレクトリ構造

```
~/.emacs.d/
├── early-init.el       # 起動高速化（GC調整、UI事前無効化）※通常のelisp
├── init.el            # 最小限のブートストラップ（config.orgをtangle/load）
├── config.org         # メイン設定ファイル（文芸的プログラミング）
├── config.el          # [自動生成] config.orgからtangle（.gitignore）
├── modules/           # [自動生成] config.orgから必要に応じてtangle
│   ├── core.el        # 基本設定（エンコーディング、バックアップ等）
│   ├── completion.el  # Corfu/Vertico/Consult設定
│   ├── editor.el      # Evil/General.el/Which-key設定
│   ├── lsp.el         # Eglot設定 + format on save
│   ├── languages.el   # 言語別設定（Go、Haskell、TypeScript等）
│   ├── tools.el       # Magit/Treemacs/diff-hl設定
│   ├── terminal.el    # Vterm設定
│   └── ui.el          # doom-themes/フォント/modeline設定
├── backups/           # 番号付きバックアップ（自動生成）
├── auto-saves/        # 自動保存ファイル（自動生成）
├── undo-tree/         # undo履歴（自動生成）
└── elpaca/            # パッケージ保存先（自動生成）
```

**注意**: `config.el`と`modules/*.el`は`config.org`からtangleで生成されるため、gitには含めない

## 5. 実装ステップ

### Step 0: 事前準備
```bash
# 現在のDoom設定をバックアップ
mv ~/.emacs.d ~/.emacs.d.doom-backup

# 新規ディレクトリ作成
mkdir -p ~/.emacs.d/modules
```

### Step 1: early-init.el（起動高速化）
**目的**: UIの事前無効化とGC最適化で起動を高速化

```elisp
;; GCを一時的に無効化（起動後に戻す）
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; UIを事前に無効化
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; ネイティブコンパイル設定
(with-eval-after-load 'comp
  (setopt native-comp-async-report-warnings-errors nil))
```

### Step 2: init.el（最小限のブートストラップ）
**目的**: config.orgをtangle/loadするだけの最小限コード

```elisp
;;; init.el --- Bootstrap configuration -*- lexical-binding: t -*-

(let* ((org-file (expand-file-name "config.org" user-emacs-directory))
       (el-file (expand-file-name "config.el" user-emacs-directory)))
  
  ;; .elファイルが存在しない、または.orgの方が新しい場合はtangle
  (when (or (not (file-exists-p el-file))
            (file-newer-than-file-p org-file el-file))
    (require 'org)
    (message "Tangling config.org...")
    (org-babel-tangle-file org-file)
    (message "Tangling complete!"))
  
  ;; tangleされたファイルをload
  (load el-file nil t))
```

### Step 3: config.org（メイン設定ファイル）
**目的**: すべての設定を文芸的プログラミングで管理

```org
#+TITLE: Emacs Configuration
#+AUTHOR: y.kasa
#+PROPERTY: header-args:emacs-lisp :tangle yes :mkdirp yes :comments link

* はじめに
この設定ファイルはDoom EmacsからVanilla Emacsへの移行を目的としています。
起動速度0.5秒以内を目指しつつ、必要な機能をすべて備えた設定です。

* パッケージ管理（Elpaca）
** なぜElpacaを選んだか
straight.elの作者が開発した後継であり、
非同期並列処理による高速インストールが可能です。

#+begin_src emacs-lisp
  ;; Elpacaブートストラップ
  (defvar elpaca-installer-version 0.7)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  ;; ... Elpacaの初期化コード
  
  ;; use-package統合
  (elpaca elpaca-use-package
    (elpaca-use-package-mode))
#+end_src

* 基本設定
** エンコーディング
日本語環境での文字化けを防ぐための設定です。

#+begin_src emacs-lisp :tangle modules/core.el
  ;;; modules/core.el --- Core settings -*- lexical-binding: t -*-
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
#+end_src

** バックアップ設定
VSCodeのTimeline機能のように、ファイルの過去バージョンを管理します。

#+begin_src emacs-lisp :tangle modules/core.el
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

* モジュールのロード
分割したモジュールを読み込みます。

#+begin_src emacs-lisp
  (dolist (module '("core" "editor" "completion" "lsp" "languages" "tools" "terminal" "ui"))
    (load (expand-file-name (format "modules/%s.el" module) user-emacs-directory)))
#+end_src
```

### Step 4: 各モジュールの設定をconfig.orgに記述
**目的**: 各機能を文芸的プログラミングで管理

config.orgの続き：

```org
* エディタ設定
** Evil（Vimキーバインド）
Vimの操作性を実現します。なぜVimキーバインドかというと、
モーダル編集は効率的で、キーボードから手を離さずに操作できるためです。

#+begin_src emacs-lisp :tangle modules/editor.el
  ;;; modules/editor.el --- Editor settings -*- lexical-binding: t -*-
  
  (use-package evil
    :ensure t
    :custom
    (evil-want-integration t)
    (evil-want-keybinding nil)
    :config
    (evil-mode 1))
#+end_src

** リーダーキー設定
SPCをリーダーキーとして使用し、VSCodeのCommand Paletteのような操作を実現します。

#+begin_src emacs-lisp :tangle modules/editor.el
  (use-package general
    :ensure t
    :config
    (general-create-definer leader-def
      :states '(normal visual insert emacs)
      :prefix "SPC"
      :non-normal-prefix "M-SPC"))
  
  (use-package which-key
    :ensure t
    :defer 1
    :config
    (which-key-mode))
#+end_src

** Undo Tree
視覚的なundo履歴管理により、変更履歴を樹形図で確認できます。

#+begin_src emacs-lisp :tangle modules/editor.el
  (use-package undo-tree
    :ensure t
    :custom
    (undo-tree-auto-save-history t)
    (undo-tree-history-directory-alist
     `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory))))
    :config
    (global-undo-tree-mode 1)
    (evil-set-undo-system 'undo-tree))
#+end_src
```

## 6. 文芸的プログラミングによる設定管理

### なぜorg-modeを使うか

1. **設定の「なぜ」を文書化**：各設定の背景や理由を記述できる
2. **折りたたみによる見通しの良さ**：大規模設定でも管理しやすい
3. **HTMLエクスポート**：美しいドキュメントとして共有可能
4. **単一の情報源**：config.orgが唯一の真実

### tangleの仕組み

org-babelのtangle機能により、orgファイル内のコードブロックを実際のelispファイルに出力します：

1. `:tangle yes` - 現在のファイル（config.el）に出力
2. `:tangle modules/xxx.el` - 指定したファイルに出力
3. `:tangle no` - tangleしない（ドキュメント専用）

### 開発フロー

```mermaid
graph LR
    A[config.org編集] --> B[保存]
    B --> C[init.elがtangleチェック]
    C --> D[必要ならtangle実行]
    D --> E[config.el生成]
    E --> F[Emacs起動/再起動]
```

### パフォーマンス保証

- **初回起動**：tangleが必要（数秒）
- **2回目以降**：0.5秒以内（tangleされた.elを直接読込）
- **開発時**：org変更時のみ再tangle

## 7. gitignore設定

```gitignore
# Generated files from org-tangle
config.el
modules/*.el

# Keep these files
!early-init.el
!init.el

# Emacs artifacts
*~
\#*\#
*.elc
.emacs.d/backups/
.emacs.d/auto-saves/
.emacs.d/undo-tree/
.emacs.d/elpaca/

# OS files
.DS_Store
```

## 8. パフォーマンス最適化

### 起動時間の内訳目標
```
early-init.el:     0.05秒（GC調整、UI無効化）
Elpaca初期化:      0.10秒（非同期ロード）
Evil/キーバインド: 0.15秒（遅延ロード）
補完システム:      0.10秒（オンデマンド）
UI/テーマ:        0.10秒（最終段階）
合計:             0.50秒以内
```

### 最適化テクニック
1. **遅延ロード徹底**: `:defer t`を活用
2. **フック活用**: 必要時のみ有効化
3. **ネイティブコンパイル**: Emacs28+の機能活用
4. **非同期処理**: Elpacaの並列インストール

## 9. 移行時の注意点

### データ移行
- **Copilot設定**: 認証情報の再設定が必要
- **プロジェクト履歴**: projectileからproject.elへ
- **カスタムスニペット**: `~/.config/doom/snippets/`からコピー
- **バックアップ設定**: `.gitignore`に追加必要
  ```gitignore
  *~
  \#*\#
  .emacs.d/backups/
  .emacs.d/auto-saves/
  .emacs.d/undo-tree/
  ```

### 互換性の課題
- **Doom独自マクロ**: `after!`、`map!`等は使えない
- **パッケージ名の違い**: 一部パッケージ名が異なる
- **キーバインド**: 完全互換ではないため調整必要

### 変数設定の注意事項（Emacs 29以降）
- **カスタム変数（defcustom）**: 
  - use-package内では`:custom`キーワードを使用
  - use-package外では`with-eval-after-load`と`setopt`を組み合わせる
  - 型チェックと`:set`関数の実行が保証される
- **非カスタム変数（defvar）**: 
  - `setq`を使用（従来通り）
  - gc-cons-threshold等のシステム変数はこちら
- **`setopt`の注意点**:
  - 必ず対象パッケージのロード後に実行
  - `with-eval-after-load`で包むことでロード順を保証

### トラブルシューティング
1. **起動エラー時**: `emacs --debug-init`で詳細確認
2. **パッケージエラー**: `M-x elpaca-manager`で状態確認
3. **LSPが動かない**: 各言語のLSPサーバーインストール確認
4. **フォーマットエラー**: `M-x eglot-events-buffer`でLSPログ確認

## 10. 必須機能チェックリスト

### 基本機能
- [ ] Emacsが0.5秒以内に起動する
- [ ] Evil modeのキーバインドが正常動作
- [ ] `SPC`でWhich-keyメニューが表示

### ファイル操作
- [ ] `SPC f f`でファイル検索が動作
- [ ] `SPC f h`でファイル履歴（backup-walker）が表示
- [ ] `SPC p p`でプロジェクト切替が動作

### 開発環境
- [ ] Go/Haskell/TypeScriptでLSPが自動起動
- [ ] 保存時に自動フォーマットが実行される
- [ ] import文が自動整理される（Go）

### バージョン管理
- [ ] Magitが`SPC g g`で起動
- [ ] Treemacsでgitステータスが表示される
- [ ] diff-hlで変更箇所がハイライト

### バックアップ
- [ ] ファイル保存時に番号付きバックアップが作成される
- [ ] backup-walkerで過去バージョンが閲覧できる
- [ ] undo-treeで視覚的なundo履歴が表示される

### その他
- [ ] Vtermが`SPC o t`で起動
- [ ] Copilotの補完が動作
- [ ] テーマとフォントが正しく適用

## 11. 段階的移行戦略

### Phase 1: 最小構成（1日目）
- early-init.el + init.el
- Evil + 基本キーバインド
- 最低限の編集機能

### Phase 2: 開発環境（2-3日目）
- 補完システム
- LSP設定 + Format on Save
- 主要言語サポート

### Phase 3: 完全移行（4-7日目）
- バックアップシステム
- すべてのツール
- カスタマイズ調整
- 最適化

## 12. 今後の拡張計画

- **org-mode設定**: GTD、ノート管理
- **メール統合**: mu4e等
- **AI補完強化**: Copilot Chat
- **デバッグ環境**: dap-mode
- **さらなる最適化**: lazy-load-eval等

## 13. 参考リンク

### 日本語記事（apribase.net）
- [Emacs + Evil 2023年版設定](https://apribase.net/2023/10/22/emacs-evil-2023/)
- [MacでEmacsをデーモンとして使う](https://apribase.net/2024/06/21/emacs-as-daemon-for-mac/)
- [Vertico + Consult設定](https://apribase.net/2024/07/18/emacs-vertico-consult/)
- [Moody + mlscrollでモードライン改善](https://apribase.net/2024/07/19/emacs-moody-mlscroll/)
- [Nerd Icons設定](https://apribase.net/2024/07/20/emacs-nerd-icons/)
- [自動保存設定](https://apribase.net/2024/07/22/emacs-auto-save/)
- [Emacs最適化テクニック](https://apribase.net/2024/07/23/emacs-optimize/)
- [基本設定まとめ](https://apribase.net/2024/07/24/emacs-base-settings/)
- [Mac言語環境設定](https://apribase.net/2024/07/26/emacs-language-environment-mac/)
- [モダンEmacs 2024](https://apribase.net/2024/07/27/modern-emacs-2024/)

---

## 14. EMACS_BOOK.md 重複パッケージ問題と章整理計画

### 📊 現状分析

#### 🔥 発見された重複パッケージ
1. **`consult`**: Line 806（基本設定）+ Line 863（ripgrep設定）
2. **`magit`**: Line 1075（第6章実装）+ Line 1582（第10章遅延読み込み例）  
3. **`treemacs`**: Line 1091（第6章実装）+ Line 1587（第10章遅延読み込み例）
4. **`doom-themes`**: Line 423（第3章基本）+ Line 1299（第8章詳細）

#### 📋 ハイブリッド方式キーバインドの問題
- 基本グループ構造は第4章で一元定義済み
- 各機能の詳細は各パッケージで分散定義済み
- しかし重複パッケージにより設定が競合している

### 🎯 修正計画

#### **Phase 1: 重複パッケージの統合**

##### 1.1 Consultの統合（第5章）
```elisp
# 修正前: 2箇所に分散
(use-package consult :bind (...))  # Line 806
(use-package consult :config (...)) # Line 863

# 修正後: 1箇所に統合
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line))
  :config
  ;; ripgrep設定も統合
  (setopt consult-ripgrep-args
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  ;; 検索関連キーバインド
  (with-eval-after-load 'general
    (general-def
      :states '(normal visual)
      :prefix "SPC"
      "/" '(consult-ripgrep :which-key "search project"))
    (general-def
      :states '(normal visual)
      :prefix "SPC s"
      "p" '(consult-ripgrep :which-key "search project")
      "l" '(consult-line :which-key "search lines"))))
```

##### 1.2 Doom-themesの統合（第8章に移動）
```elisp
# 第3章から削除: Line 423-426
# 第8章に統合: より詳細な設定として

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
```

##### 1.3 第10章の遅延読み込み例を明確化
```elisp
# 修正前: 実装と例が混在
(use-package magit :ensure t :bind (...)) # Line 1075 実装
(use-package magit :defer t :commands (...)) # Line 1582 例

# 修正後: 例であることを明記
** 遅延読み込み戦略（設定例）
以下は遅延読み込みの設定例です。実際の設定は各章で行われています。

#+begin_src emacs-lisp :tangle no
  ;; 重いパッケージの遅延読み込み例（実装は各章参照）
  (use-package magit
    :defer t
    :commands (magit-status magit-clone))
#+end_src
```

#### **Phase 2: 章構成の最適化**

##### 2.1 第3章: エディタ基礎編
- 基本的な編集機能のみに集中
- doom-themes設定を第8章に移動
- smartparens、ace-window、helpful等は維持

##### 2.2 第5章: 補完システム編
- consult設定を完全統合
- vertico、orderless、marginalia、corfu設定は維持
- 検索・置換機能をここに集約

##### 2.3 第8章: UI/UX改善編
- doom-themes設定をここに移動・拡張
- doom-modeline、nerd-icons等と合わせて統合
- テーマ関連の全設定を一元化

##### 2.4 第10章: パフォーマンス最適化編
- 重複パッケージ設定を削除
- 遅延読み込み例であることを明記（:tangle no使用）
- ベンチマークとプロファイリングツールに集中

#### **Phase 3: ファイル構造の改善**

##### 3.1 設定の論理的グループ化
```
第1章: 基盤（early-init, init, 基本設定）
第2章: パッケージ管理（Elpaca, use-package）
第3章: エディタ基礎（編集支援機能のみ）
第4章: Evil（Vimキーバインド + 基本キー構造）
第5章: 補完システム（検索・補完の完全統合）
第6章: 開発環境（LSP, Git, プロジェクト管理）
第7章: ターミナル統合（Vterm関連）
第8章: UI/UX（テーマ・見た目の完全統合）
第9章: 言語別設定（プログラミング言語）
第10章: パフォーマンス最適化（ベンチマーク重視）
第11章: Org-mode活用（ドキュメント・タスク管理）
```

##### 3.2 設定原則の確立
1. **1パッケージ1箇所**: 同一パッケージの設定は1箇所にまとめる
2. **機能別グループ化**: 関連機能は同じ章にまとめる
3. **例と実装の分離**: 例は`:tangle no`で明確に区別
4. **キーバインドの統一**: ハイブリッド方式を維持

### 📅 実装タイムライン

#### Week 1: 重複解消
- Day 1: consult統合
- Day 2: doom-themes移動
- Day 3: 第10章例の明確化

#### Week 2: 章最適化
- Day 1-2: 第3章・第8章の再構成
- Day 3-4: 第5章の完全統合
- Day 5: 第10章の純化

#### Week 3: 品質保証
- Day 1-2: 全章の整合性チェック
- Day 3-4: 実際の設定での動作確認
- Day 5: ドキュメント最終調整

### 🎯 期待される効果

1. **設定の一意性**: 各パッケージが1箇所でのみ定義される
2. **保守性向上**: 重複による混乱が解消される
3. **学習効率**: 論理的な章構成により理解しやすくなる
4. **実用性**: 実装と例が明確に分離される

---

根拠：この計画は、あなたの要望（起動速度重視、VSCodeライク、段階的移行、Format on Save、VSCode Timeline相当のバックアップ）をすべて満たし、Doom Emacsの良い部分を維持しつつ、完全に制御可能な環境を実現します。