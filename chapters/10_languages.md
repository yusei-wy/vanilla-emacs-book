[← 前の章へ](09_ui.md) | [目次に戻る](00_introduction.md) | [次の章へ →](11_performance.md)

---

# 第9章：言語別設定編 - 各言語への最適化

[← 前の章へ](09_ui.md) | [目次に戻る](00_introduction.md) | [次の章へ →](11_performance.md)

---

## この章の目標
- 各プログラミング言語の専用モード設定
- 言語固有の便利機能とインデント設定
- 構造化された設定で将来の拡張に対応

## 第9章の設定：言語別カスタマイズ

```org
* lang

** Tree-sitter: 高速・高精度なシンタックス解析

Tree-sitter による構文解析を有効化。従来の正規表現ベースより高速かつ正確。

*** 使い方

初回起動時に各言語のインストール確認プロンプトが表示されます。

**手動インストール**: ~M-x treesit-install-language-grammar~

#+begin_src emacs-lisp
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))

    (setq treesit-language-source-alist
          '((typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                       "v0.23.2" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                 "v0.23.2" "tsx/src")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                        "v0.25.0" "src")
            (json "https://github.com/tree-sitter/tree-sitter-json"
                  "v0.24.8" "src")
            (css "https://github.com/tree-sitter/tree-sitter-css"
                 "v0.25.0" "src")
            (html "https://github.com/tree-sitter/tree-sitter-html"
                  "v0.23.2" "src")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml"
                  "v0.5.0" "src")
            (go "https://github.com/tree-sitter/tree-sitter-go"
                "v0.25.0" "src")
            (gomod "https://github.com/camdencheek/tree-sitter-go-mod"
                   "v1.0.2" "src")
            (haskell "https://github.com/tree-sitter/tree-sitter-haskell"
                     "v0.23.1" "src")))

    (unless noninteractive
      (dolist (lang treesit-language-source-alist)
        (unless (treesit-language-available-p (car lang))
          (when (y-or-n-p (format "Install tree-sitter grammar for %s? " (car lang)))
            (treesit-install-language-grammar (car lang))))))

    (setq major-mode-remap-alist
          '((typescript-mode . typescript-ts-mode)
            (js-mode . js-ts-mode)
            (javascript-mode . js-ts-mode)
            (js2-mode . js-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (yaml-mode . yaml-ts-mode)
            (go-mode . go-ts-mode))))
#+end_src

** languages

### プログラミング言語

#### Go言語

#+begin_src emacs-lisp
  ;; go-ts-mode のインデント設定
  ;; デフォルト値8だと、tab-width 4 で2タブ挿入されるため、4に統一
  ;; 参考: https://mail.gnu.org/archive/html/emacs-devel/2023-09/msg01353.html
  (setq go-ts-mode-indent-offset 4)

  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"
    :commands (my/go-test-current-test
               my/go-test-file
               my/go-test-all
               my/go-test-rerun
               my/go-test-clean-cache)
    :hook ((go-mode . (lambda ()
                        (setq tab-width 4)
                        (setq indent-tabs-mode t))))
    :init
    ;; テスト実行のヘルパー関数
    (defvar my/go-test-last nil
      "最後に実行したテストコマンド")

    (defun my/go--run-tests (args)
      "Run go test with ARGS."
      (let ((cmd (concat "go test -v " args)))
        (setq my/go-test-last cmd)
        (compile cmd)))

    (defun my/go-test-all ()
      "Run all tests in current project."
      (interactive)
      (my/go--run-tests ""))

    (defun my/go-test-file ()
      "Run all tests in current file."
      (interactive)
      (my/go--run-tests (buffer-file-name)))

    (defun my/go-test-current-test ()
      "Run test at point."
      (interactive)
      (save-excursion
        (re-search-backward "^func[ ]+\\(Test[[:alnum:]_]+\\)")
        (my/go--run-tests (concat "-run " (match-string 1)))))

    (defun my/go-test-rerun ()
      "Rerun last test."
      (interactive)
      (if my/go-test-last
          (compile my/go-test-last)
        (my/go-test-all)))

    (defun my/go-test-clean-cache ()
      "Clean Go test cache."
      (interactive)
      (compile "go clean -testcache"))

    ;; キーバインド（グローバル）
    (with-eval-after-load 'general
      (leader-def
        "t t" '(my/go-test-current-test :which-key "test at point")
        "t f" '(my/go-test-file :which-key "test file")
        "t p" '(my/go-test-all :which-key "test package")
        "t r" '(my/go-test-rerun :which-key "rerun last test")
        "t C" '(my/go-test-clean-cache :which-key "clean test cache")))
    :config
    ;; gofmtの代わりにgoimportsを使用（自動import整理）
    (setq gofmt-command "goimports"))

  ;; テスト生成
  (use-package go-gen-test
    :ensure t
    :commands (go-gen-test-dwim go-gen-test-all)
    :init
    (with-eval-after-load 'general
      (leader-def
        "t g" '(go-gen-test-dwim :which-key "generate test")
        "t G" '(go-gen-test-all :which-key "generate all tests"))))
#+end_src

#### Haskell
```emacs-lisp
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :custom
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-starter-offset 4)
  (haskell-indentation-left-offset 4))
```

#### TypeScript/JavaScript
```emacs-lisp
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
```

### マークアップ・スタイルシート言語

#### Web開発（HTML/CSS）
```emacs-lisp
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
         (css-mode . emmet-mode)
         (typescript-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (js2-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t))  ; React対応
```

### データ形式

#### JSON
```emacs-lisp
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.jsonc\\'")
  :hook (json-mode . (lambda ()
                       (setq-local js-indent-level 2))))
```

#### YAML
```emacs-lisp
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
```

#### Markdown
```emacs-lisp
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "markdown"))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :custom
  (markdown-preview-stylesheets
   (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css"))
  :config
  (with-eval-after-load 'general
    (general-define-key
     :states '(normal visual)
     :keymaps 'markdown-mode-map
     :prefix "SPC"
     "m" '(:ignore t :which-key "markdown")
     "mp" '(markdown-preview-mode :which-key "toggle preview")
     "mo" '(markdown-preview-open-browser :which-key "open in browser")
     "ml" '(markdown-insert-link :which-key "insert link")
     "mi" '(markdown-insert-image :which-key "insert image")
     "mc" '(markdown-insert-gfm-code-block :which-key "insert code block"))))
```

### 共通設定

#### フォーマッター統合
```emacs-lisp
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
```

#### Tree-sitter（構文ハイライト強化）
```emacs-lisp
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
```
```

### この章で得られたもの
- ✅ Go, Haskell, TypeScript/JavaScript, HTML/CSS, JSON, YAML, Markdownの完全サポート
- ✅ 各言語に最適化されたインデント設定
- ✅ web-modeによる統合的なWeb開発環境
- ✅ Emmetによる高速HTML/CSSコーディング
- ✅ Markdownのリアルタイムプレビュー（GitHub風スタイル）
- ✅ `SPC m`でMarkdown編集機能に素早くアクセス
- ✅ format-allによる統一的なコードフォーマット
- ✅ Tree-sitterによる高精度な構文ハイライト（Emacs 29+）
- ✅ 構造化された設定で新しい言語の追加が容易

---

[← 前の章へ](09_ui.md) | [目次に戻る](00_introduction.md) | [次の章へ →](11_performance.md)