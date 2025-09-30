[← 前の章へ](05_evil.md) | [目次に戻る](00_introduction.md) | [次の章へ →](07_development.md)

---

# 第5章：補完システム編 - 賢い入力支援

### 🎯 この章の目標
- 高速な補完システムの構築
- ファジー検索の導入
- コマンド実行の効率化

### 📝 実装内容

```org
* completion

** vertico-stack
#+begin_src emacs-lisp
  (use-package vertico
    :ensure t
    :init
    (vertico-mode)
    :custom
    (vertico-cycle t))
#+end_src

*** orderless
#+begin_src emacs-lisp
  (use-package orderless
    :ensure t
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))
#+end_src

*** marginalia
#+begin_src emacs-lisp
  (use-package marginalia
    :ensure t
    :init
    (marginalia-mode))
#+end_src

** consult
#+begin_src emacs-lisp
  (use-package consult
    :ensure t
    :init
    ;; SPCキーバインドを事前定義
    (with-eval-after-load 'general
      (leader-def
        "/" '(consult-ripgrep :which-key "search project")
        "s" '(:ignore t :which-key "search")
        "s p" '(consult-ripgrep :which-key "search project")
        "s l" '(consult-line :which-key "search lines")))
    :bind (("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("M-y" . consult-yank-pop)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line))
    :config
    ;; ripgrepの詳細設定
    (setopt consult-ripgrep-args
            "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))
#+end_src

** project
#+begin_src emacs-lisp
  ;; projectile: プロジェクト管理
  (use-package projectile
    :ensure t
    :init
    (projectile-mode 1)
    ;; プロジェクト関連キーバインド
    (with-eval-after-load 'general
      (leader-def
        "p r" '(projectile-replace :which-key "replace in project")
        "p R" '(projectile-replace-regexp :which-key "replace regexp")))
    :custom
    (projectile-completion-system 'default)
    (projectile-enable-caching t))

  ;; consult-projectile: consultとの統合
  (use-package consult-projectile
    :ensure t
    :after (consult projectile)
    :init
    ;; SPCキーバインドを事前定義
    (with-eval-after-load 'general
      (leader-def
        "f p" '(consult-projectile :which-key "quick open")
        "p f" '(consult-projectile-find-file :which-key "find file")
        "p p" '(consult-projectile-switch-project :which-key "switch project"))))
#+end_src

** multiple-cursors
#+begin_src emacs-lisp
  (use-package evil-mc
    :ensure t
    :after evil
    :init
    (with-eval-after-load 'general
      (general-define-key
       :states '(normal visual)
       "C-d" 'evil-mc-make-and-goto-next-match    ; 次の単語を選択
       "C-S-d" 'evil-mc-skip-and-goto-next-match ; スキップして次へ
       "C-M-d" 'evil-mc-make-all-cursors)        ; 全て選択

      (leader-def
        "m a" '(evil-mc-make-all-cursors :which-key "select all")
        "m n" '(evil-mc-make-and-goto-next-match :which-key "next match")
        "m p" '(evil-mc-make-and-goto-prev-match :which-key "prev match")
        "m q" '(evil-mc-undo-all-cursors :which-key "quit")))
    :config
    (global-evil-mc-mode 1))
#+end_src

** corfu
#+begin_src emacs-lisp
  (use-package corfu
    :ensure t
    :custom
    (corfu-auto t)
    (corfu-auto-delay 0.0)
    (corfu-auto-prefix 2)
    :init
    (global-corfu-mode))

  ;; Cape: 追加の補完機能
  (use-package cape
    :ensure t
    :after corfu
    :config
    ;; ファイルパス補完を追加
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;; 単語補完を追加
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))
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
- ✅ ファイルパス補完（Cape）

---

[← 前の章へ](05_evil.md) | [目次に戻る](00_introduction.md) | [次の章へ →](07_development.md)
