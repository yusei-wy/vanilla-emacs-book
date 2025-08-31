## 付録D：パフォーマンス測定ガイド

### 起動時間の測定

#### 基本的な測定方法
```bash
# コマンドラインから測定
time emacs --eval "(kill-emacs)"

# より詳細な測定
emacs --eval "(progn (message \"Load time: %.2fs\" (float-time (time-subtract after-init-time before-init-time))) (kill-emacs))"
```

#### esupによる詳細分析
```elisp
;; M-x esup でプロファイル実行
;; 各パッケージの読み込み時間が表示される

;; 結果の見方:
;; Total time: 全体の起動時間
;; gc-done: GC実行回数
;; 各行に読み込み時間が表示
```

### メモリ使用量の監視

```elisp
;; メモリ使用量を確認
(defun my/show-memory-usage ()
  "Display current memory usage."
  (interactive)
  (message "Memory: %.1f MB (GCs: %d)"
           (/ (car (memory-info)) 1024.0)
           gcs-done))

;; GCの統計情報
(defun my/gc-statistics ()
  "Show GC statistics."
  (interactive)
  (message "GC threshold: %s, GCs done: %d, GC time: %.2fs"
           gc-cons-threshold
           gcs-done
           gc-elapsed))
```

### パッケージごとの測定

```elisp
;; use-package統計を有効化
(setq use-package-compute-statistics t)

;; 統計を表示
M-x use-package-report

;; 結果の見方:
;; Package: パッケージ名
;; Status: 読み込み状態
;; Last Load Time: 最後の読み込み時間
;; Total Load Time: 合計読み込み時間
```

### プロファイリング実践

#### CPUプロファイル
```elisp
M-x profiler-start RET cpu RET
;; 測定したい操作を実行
M-x profiler-report
;; + をクリックして詳細表示
;; 時間がかかっている関数を特定
M-x profiler-stop
```

#### メモリプロファイル
```elisp
M-x profiler-start RET memory RET
;; メモリを消費する操作を実行
M-x profiler-report
;; メモリ使用量の多い関数を確認
M-x profiler-stop
```

### 最適化のベンチマーク

```elisp
;; ベンチマーク関数
(defun my/benchmark-config ()
  "Benchmark current configuration."
  (interactive)
  (let ((start-time (current-time)))
    ;; テストしたい処理
    (dotimes (i 1000)
      (+ 1 1))
    (message "Elapsed time: %.3fs"
             (float-time (time-subtract (current-time) start-time)))))

;; パッケージ読み込みのベンチマーク
(benchmark-run-compiled 100
  (require 'org))
```

### パフォーマンス改善チェックリスト

#### ✅ 起動時間の改善
- [ ] early-init.elでGC無効化
- [ ] 遅延読み込み（:defer t）の活用
- [ ] ネイティブコンパイル有効化
- [ ] 不要なパッケージの削除

#### ✅ 実行時パフォーマンス
- [ ] GCMHの導入
- [ ] 大きなファイルの警告設定
- [ ] プロセス読み込みサイズ調整
- [ ] フォント設定の最適化

#### ✅ メモリ使用量
- [ ] GC閾値の調整
- [ ] 不要なバッファの自動削除
- [ ] アイドル時のGC実行
- [ ] キャッシュサイズの制限

---

## 付録E：よくある質問（FAQ）

### Doom Emacsからの移行

**Q: Doom Emacsの設定をそのまま使える？**
A: 直接は使えませんが、本書の設定で同等の機能を実現できます。特に第4章（Evil）、第5章（補完）、第8章（UI）を参照してください。

**Q: Doom固有のマクロ（after!、map!等）の代替は？**
A: 
- `after!` → `with-eval-after-load` または `use-package :after`
- `map!` → `general-def` または `define-key`
- `load!` → `load-file` または `require`

**Q: モジュールシステムを再現したい**
A: 付録Cのモジュール化構造を参考に、org-modeファイルを分割して管理できます。

### パフォーマンス問題

**Q: 起動が3秒以上かかる**
A: 
1. `M-x esup` で遅いパッケージを特定
2. `:defer t` で遅延読み込みを設定
3. early-init.elの最適化を確認

**Q: メモリ使用量が多い（500MB以上）**
A:
1. GCMHを導入（第10章参照）
2. 不要なパッケージを削除
3. `M-x memory-report` で詳細確認

### エラー対処

**Q: "Package cl is deprecated" エラー**
A: `(require 'cl-lib)` に置き換えてください。cl-libは新しい標準ライブラリです。

**Q: Native compilation警告が大量に出る**
A: `(setq native-comp-async-report-warnings-errors nil)` を early-init.el に追加

**Q: 日本語入力が切り替わらない**
A: macOSの場合、System Preferences → Security & Privacy → Accessibility でEmacsを許可

### カスタマイズ

**Q: VSCodeのようなミニマップを追加したい**
A: `minimap` パッケージを追加:
```elisp
(use-package minimap
  :ensure t
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.1))
```

**Q: タブバーを使いたい**
A: Emacs 27+では組み込み:
```elisp
(use-package tab-bar
  :ensure nil
  :config
  (tab-bar-mode 1)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*"))
```

**Q: AIコード補完を追加したい**
A: Copilot.elを使用:
```elisp
(use-package copilot
  :elpaca (:host github :repo "copilot-emacs/copilot.el")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)))
```

---

## 付録F：推奨リソース

### 📚 必読書籍

#### 入門書
- **『Emacs実践入門』** - 大竹智也
  - 日本語で最も詳しい入門書
  - 基本操作から実践的カスタマイズまで

- **『Learning GNU Emacs』** - O'Reilly
  - 英語だが図解が豊富
  - 体系的な学習に最適

#### 上級書
- **『Mastering Emacs』** - Mickey Petersen
  - 内部構造の理解に最適
  - 高度なカスタマイズテクニック

- **『Writing GNU Emacs Extensions』** - Bob Glickstein
  - Elisp プログラミングの詳細
  - パッケージ開発者向け

### 🌐 Webサイト

#### 公式リソース
- [GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/)
  - 最も信頼できるリファレンス
  - `C-h i` でEmacs内から参照可能

- [Emacs Wiki](https://www.emacswiki.org/)
  - コミュニティ維持のWiki
  - Tips & Tricksの宝庫

#### 日本語リソース
- [るびきち「日刊Emacs」](http://rubikitch.com/tag/emacs/)
  - 日本語の貴重な情報源
  - パッケージレビューが充実

- [Qiita Emacsタグ](https://qiita.com/tags/emacs)
  - 実践的な設定例
  - 問題解決の事例集

#### 設定例
- [Doom Emacs](https://github.com/doomemacs/doomemacs)
  - モダンな設定の参考
  - 優れたパッケージ選定

- [Spacemacs](https://www.spacemacs.org/)
  - レイヤーシステムの参考
  - Evil統合の実装例

- [Centaur Emacs](https://github.com/seagle0128/.emacs.d)
  - クリーンな実装
  - クロスプラットフォーム対応

### 🎥 動画リソース

- **System Crafters** (YouTube)
  - Emacs From Scratchシリーズ
  - ライブコーディング形式

- **Protesilaos Stavrou** (YouTube)
  - 高度なカスタマイズ
  - パッケージ開発

- **Mike Zamansky** (YouTube)
  - 教育的アプローチ
  - 段階的な学習

### 💬 コミュニティ

#### Discord/Slack
- **Emacs Japan** (Slack)
  - 日本語コミュニティ
  - 初心者歓迎

- **System Crafters** (Discord)
  - 国際コミュニティ
  - アクティブなサポート

#### Reddit
- [r/emacs](https://www.reddit.com/r/emacs/)
  - 最新情報の共有
  - Weekly Tips Thread

#### Stack Overflow
- [Emacs Stack Exchange](https://emacs.stackexchange.com/)
  - Q&A形式
  - 高品質な回答

### 🔧 ツール・ユーティリティ

- **chemacs2** - 複数の設定を切り替え
- **straight.el** - Elpacaの代替
- **benchmark-init** - 起動時間の詳細分析
- **explain-pause-mode** - 遅延の原因特定

---

## 索引

### 主要コマンド一覧

#### ファイル操作
- `C-x C-f` (find-file) - ファイルを開く
- `C-x C-s` (save-buffer) - ファイル保存
- `C-x C-w` (write-file) - 名前を付けて保存
- `SPC f f` - Consulでファイル検索
- `SPC f r` - 最近使ったファイル

#### バッファ操作
- `C-x b` (switch-to-buffer) - バッファ切り替え
- `C-x k` (kill-buffer) - バッファを閉じる
- `SPC b b` - Consulでバッファ選択
- `SPC b k` - 現在のバッファを閉じる

#### ウィンドウ操作
- `C-x 2` (split-window-below) - 水平分割
- `C-x 3` (split-window-right) - 垂直分割
- `C-x 0` (delete-window) - ウィンドウを閉じる
- `C-x 1` (delete-other-windows) - 他を閉じる
- `M-o` (ace-window) - ウィンドウジャンプ

#### 検索・置換
- `C-s` (isearch-forward) - 前方検索
- `C-r` (isearch-backward) - 後方検索
- `M-%` (query-replace) - 対話的置換
- `SPC s p` - プロジェクト内検索
- `SPC s b` - バッファ内検索

#### Evil特有
- `gcc` - 行コメント切り替え
- `gc` - 選択範囲コメント
- `cs"'` - 囲み文字を変更
- `ds"` - 囲み文字を削除
- `ysiw"` - 単語を囲む

#### Git操作
- `C-x g` (magit-status) - Git状態表示
- `SPC g s` - Git status
- `SPC g b` - Git blame
- `SPC g d` - Git diff

### 主要変数一覧

#### パフォーマンス
- `gc-cons-threshold` - GC実行閾値
- `gc-cons-percentage` - GC実行割合
- `read-process-output-max` - プロセス読み込みサイズ
- `large-file-warning-threshold` - 大ファイル警告

#### エンコーディング
- `buffer-file-coding-system` - ファイルエンコーディング
- `default-process-coding-system` - プロセスエンコーディング

#### バックアップ
- `make-backup-files` - バックアップ作成
- `backup-directory-alist` - バックアップ先
- `version-control` - バージョン管理
- `kept-new-versions` - 新しいバージョン保持数

#### UI/UX
- `inhibit-startup-screen` - 起動画面無効化
- `visible-bell` - ビープ音の代わりに画面フラッシュ
- `column-number-mode` - カラム番号表示
- `display-line-numbers-type` - 行番号表示形式

### パッケージ索引

#### コア機能
- **elpaca** - パッケージマネージャー
- **use-package** - パッケージ設定
- **general** - キーバインド管理
- **which-key** - キーヒント表示

#### Evil/Vim
- **evil** - Vimエミュレーション
- **evil-collection** - Evil統合
- **evil-surround** - 囲み文字操作
- **evil-commentary** - コメント操作

#### 補完
- **corfu** - 補完UI
- **cape** - 補完バックエンド
- **orderless** - あいまい検索
- **consult** - 検索インターフェース

#### 開発
- **magit** - Git統合
- **eglot** - LSPクライアント
- **treesit** - Tree-sitter統合
- **dap-mode** - デバッガー

#### UI
- **doom-themes** - テーマ集
- **doom-modeline** - モードライン
- **rainbow-delimiters** - 括弧の色分け