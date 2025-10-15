[← 前の章へ](07_development.md) | [目次に戻る](00_introduction.md) | [次の章へ →](09_ui.md)

---

# 第7章：ターミナル統合編 - シェルとの完全融合

### 🎯 この章の目標
- 完全なターミナルエミュレータの統合
- Emacs内でのシェル作業の効率化
- コードとターミナルのシームレスな連携

### 📝 実装内容

```org
* tools

** term

*** vterm
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
    ;; 全てのキーをカスタマイズ可能に
    (setq vterm-keymap-exceptions nil)

    ;; Return キーを通常通りターミナルに送る
    (define-key vterm-mode-map [return] #'vterm-send-return)

    ;; Evil連携: 起動時に Insert mode + カーソルを box に
    (add-hook 'vterm-mode-hook
      (lambda ()
        (setq-local evil-insert-state-cursor 'box)
        (evil-insert-state)))

    ;; Normal mode: Insert mode に戻るキーバインド
    (evil-define-key 'normal vterm-mode-map
      (kbd "i") #'evil-insert-state
      (kbd "a") #'evil-append
      (kbd "p") #'vterm-yank
      (kbd "u") #'vterm-undo)

    ;; Insert mode: シェルの標準操作をターミナルに送る
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-a") #'vterm--self-insert
      (kbd "C-e") #'vterm--self-insert
      (kbd "C-f") #'vterm--self-insert
      (kbd "C-b") #'vterm--self-insert
      (kbd "C-d") #'vterm--self-insert
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert
      (kbd "C-r") #'vterm--self-insert
      (kbd "C-c") #'vterm--self-insert
      (kbd "C-g") #'vterm--self-insert))

  ;; multi-vterm：複数ターミナル管理
  (use-package multi-vterm
    :ensure t
    :defer t
    :init
    ;; カスタム関数: 現在のディレクトリでvtermを開く
    (defun my/vterm-here ()
      "Open vterm in current directory."
      (interactive)
      (let ((default-directory (if (buffer-file-name)
                                   (file-name-directory (buffer-file-name))
                                 default-directory)))
        (vterm)))

    ;; キーバインド
    (with-eval-after-load 'general
      (leader-def
        "o t" '(multi-vterm :which-key "terminal")
        "o T" '(multi-vterm-dedicated-toggle :which-key "dedicated terminal")
        "o ]" '(multi-vterm-next :which-key "next terminal")
        "o [" '(multi-vterm-prev :which-key "prev terminal")
        "o h" '(my/vterm-here :which-key "terminal here")))
    :config
    (setq multi-vterm-dedicated-window-height 30))
#+end_src
```

### 💡 使い方のヒント

- **SPC o t**: 新しいターミナルを開く
- **SPC o T**: 専用ターミナルのトグル（画面下部に固定表示）
- **SPC o h**: 現在のディレクトリでターミナルを開く
- **SPC o ]** / **SPC o [**: 次/前のターミナルに切り替え
- **Vterm内でESC**: Normal modeへ（テキスト選択用）
- **Normal modeでi**: Insert modeに戻る（ターミナル操作再開）
- **Normal modeでp**: ヤンクしたテキストを貼り付け
- **Normal modeでu**: アンドゥ

### ✨ この章で得られたもの
- ✅ 完全なターミナルエミュレーション
- ✅ 複数ターミナルの管理機能
- ✅ プロジェクトごとのターミナル分離
- ✅ コードとターミナルのシームレスな切り替え
- ✅ Evilモードとの適切な統合

---

[← 前の章へ](07_development.md) | [目次に戻る](00_introduction.md) | [次の章へ →](09_ui.md)