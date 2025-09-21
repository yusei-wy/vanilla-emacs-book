# 第12章：デバッグとテスト編 - 品質の追求

[← 前の章](12_orgmode.md) | [目次](../README.md)

---

## この章の目標
- 効率的なデバッグ環境の構築
- テスト駆動開発（TDD）の実践
- REPLを活用した対話的開発
- デバッガーの完全活用

## TDDサイクル図

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

## デバッグワークフロー

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

## 第12章の設定：デバッグとテスト

### DAP-mode（デバッグアダプタープロトコル）
```emacs-lisp
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

;; デバッグ関連キーバインド
(with-eval-after-load 'general
  (general-def
    :states '(normal visual)
    :prefix "SPC d"
    "d" '(dap-debug :which-key "start debug")
    "n" '(dap-next :which-key "next")
    "i" '(dap-step-in :which-key "step in")
    "o" '(dap-step-out :which-key "step out")
    "c" '(dap-continue :which-key "continue")
    "r" '(dap-restart-frame :which-key "restart")
    "q" '(dap-quit :which-key "quit")
    "b" '(dap-breakpoint-toggle :which-key "toggle breakpoint")
    "B" '(dap-ui-breakpoints :which-key "list breakpoints")
    "e" '(dap-eval :which-key "eval")
    "E" '(dap-eval-thing-at-point :which-key "eval at point")
    "l" '(dap-ui-locals :which-key "locals")
    "s" '(dap-ui-sessions :which-key "sessions")))
```

### テストフレームワーク統合
```emacs-lisp
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
```

### REPL統合
```emacs-lisp
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
```

### Edebug（Emacs Lispデバッガー）
```emacs-lisp
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
```

### ベンチマークとプロファイリング
```emacs-lisp
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
```

### エラーハンドリング強化
```emacs-lisp
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
```

### テスト駆動開発支援
```emacs-lisp
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
```

### カバレッジ表示
```emacs-lisp
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
```

## 使い方のヒント

### デバッグワークフロー
1. `SPC d b` でブレークポイントを設定
2. `SPC d d` でデバッグセッション開始
3. `SPC d n/i/o` でステップ実行
4. `SPC d e` で式を評価
5. `SPC d l` でローカル変数を確認

### TDDサイクル
1. テストを書く（失敗することを確認）
2. 最小限のコードで実装
3. テストが通ることを確認
4. リファクタリング
5. `M-x my/auto-test-mode` で自動テスト有効化

### REPL活用
1. 言語に応じたREPLを起動
2. コードを選択して `C-c C-r` で送信
3. 対話的に結果を確認
4. `C-c C-z` でREPLバッファに切り替え

### この章で得られたもの
- ✅ VSCode風のビジュアルデバッグ環境
- ✅ 多言語対応のデバッグアダプター
- ✅ 各言語のテストフレームワーク統合
- ✅ TDDワークフローの自動化
- ✅ REPL駆動開発の環境
- ✅ コードカバレッジの可視化
- ✅ パフォーマンスプロファイリング
- ✅ 高度なエラーハンドリング

---

[← 前の章](12_orgmode.md) | [目次](../README.md)