[← 前の章へ](11_performance.md) | [目次に戻る](00_introduction.md) | [次の章へ →](13_debug_test.md)

---

# 第11章：Org-mode活用編 - 最強のドキュメントシステム

[← 前の章へ](11_performance.md) | [目次に戻る](00_introduction.md) | [次の章へ →](13_debug_test.md)

---

## この章の目標
- Org-modeの基本から応用まで習得
- GTDタスク管理システムの構築
- 文芸的プログラミングの実践
- ナレッジベースの構築

## Org-modeワークフロー図

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

## タスクのライフサイクル

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

## 第11章の設定：Org-mode活用

### 基本設定とキーバインド
```emacs-lisp
(use-package org
  :ensure nil  ; 組み込みパッケージ
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

;; Org-mode関連キーバインド
(with-eval-after-load 'general
  (leader-def
    "n" '(:ignore t :which-key "notes")
    "n n" '(org-capture :which-key "capture")
    "n a" '(org-agenda :which-key "agenda")
    "n l" '(org-store-link :which-key "store link")))
```

### Org-agenda（GTDタスク管理）
```emacs-lisp
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
```

### Org-capture（素早いメモ）
```emacs-lisp
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
```

### Org-roam（ナレッジベース）
```emacs-lisp
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
```

### Org-modern（モダンな見た目）
```emacs-lisp
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
```

### Org-download（画像の貼り付け）
```emacs-lisp
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
```

### Org-present（プレゼンテーション）
```emacs-lisp
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
```

### エクスポート設定
```emacs-lisp
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
```

## 使い方のヒント

### GTDワークフロー
1. `SPC n n` で素早くタスクをキャプチャ
2. `SPC n a` でアジェンダビューを開く
3. `d` でダッシュボードビューを表示
4. タスクを整理して優先順位をつける

### ナレッジベース構築
1. `C-c n f` でノートを検索・作成
2. `C-c n i` で他のノートへのリンクを挿入
3. `C-c n l` でバックリンクを表示
4. `M-x org-roam-ui-open` でグラフを可視化

### 文芸的プログラミング
1. `#+begin_src` でコードブロックを作成
2. `C-c C-c` でコードを実行
3. `C-c '` でコードブロックを別バッファで編集
4. `C-c C-e` でエクスポート

### この章で得られたもの
- ✅ GTDベースのタスク管理システム
- ✅ Org-captureによる素早いメモ取り
- ✅ Org-roamによるZettelkasten風ナレッジベース
- ✅ 文芸的プログラミングの実践環境
- ✅ プレゼンテーション作成機能
- ✅ 美しいドキュメントのエクスポート
- ✅ 画像の簡単な貼り付けと管理
- ✅ モダンで見やすいOrg文書

---

[← 前の章へ](11_performance.md) | [目次に戻る](00_introduction.md) | [次の章へ →](13_debug_test.md)