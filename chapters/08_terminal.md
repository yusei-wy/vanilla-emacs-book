[[file:07_development.md][← 前の章へ]] | [[file:00_introduction.md][目次に戻る]] | [[file:09_ui.md][次の章へ →]]

---

* 第7章：ターミナル統合編 - シェルとの完全融合

** 🎯 この章の目標
- 完全なターミナルエミュレータの統合
- Emacs内でのシェル作業の効率化
- コードとターミナルのシームレスな連携

** 📝 実装内容

*** 第7章の設定：ターミナル統合

**** Vterm（完全ターミナルエミュレーション）
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

  ;; ターミナル関連キーバインド
  (with-eval-after-load 'general
    (general-def
      :states '(normal visual)
      :prefix "SPC o"
      "t" '(multi-vterm :which-key "terminal")
      "T" '(multi-vterm-dedicated-toggle :which-key "dedicated terminal")
      "n" '(multi-vterm-next :which-key "next terminal")
      "p" '(multi-vterm-prev :which-key "prev terminal")))
#+end_src

**** ターミナルとの連携強化
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

**** ターミナル便利機能
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

  ;; ターミナル便利機能のキーバインド
  (with-eval-after-load 'general
    (general-def
      :states '(normal visual)
      :prefix "SPC o"
      "h" '(my/vterm-here :which-key "terminal here")
      "r" '(my/run-in-vterm :which-key "run command")))
#+end_src

** 💡 使い方のヒント

- *C-`*: ターミナルのトグル（表示/非表示）
- *SPC o t*: 新しいターミナルを開く
- *SPC o T*: 専用ターミナルのトグル
- *SPC o h*: 現在のディレクトリでターミナルを開く
- *Vterm内でC-c C-t*: Evilモードへ切り替え（テキスト選択用）

** ✨ この章で得られたもの
- ✅ 完全なターミナルエミュレーション
- ✅ 複数ターミナルの管理機能
- ✅ プロジェクトごとのターミナル分離
- ✅ コードとターミナルのシームレスな切り替え
- ✅ Evilモードとの適切な統合

---

[[file:07_development.md][← 前の章へ]] | [[file:00_introduction.md][目次に戻る]] | [[file:09_ui.md][次の章へ →]]