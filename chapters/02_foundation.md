[← 前の章へ](01_installation.md) | [目次に戻る](00_introduction.md) | [次の章へ →](03_package.md)

---

# 第1章：基礎編 - 高速起動と最小限のEmacs設定

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

[← 前の章へ](01_installation.md) | [目次に戻る](00_introduction.md) | [次の章へ →](03_package.md)