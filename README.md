# 📖 My Emacs Journey: Doom脱却からVanilla Emacsマスターへの道

## 概要

このリポジトリは、Doom Emacsから独自のVanilla Emacs環境を構築していく旅の記録です。
各章を進めるごとに、あなたのEmacsは少しずつ成長し、最終的には完全にカスタマイズされた、世界でただ一つのエディタになります。

## 📚 目次

### [はじめに](chapters/00_introduction.md)
- この本の読み方
- 各章の概要と依存関係

### 基礎編
1. [第0章：準備編 - macOSへのEmacsインストール](chapters/01_installation.md)
   - Emacs Mac Port（EMP版）のインストール
   - 日本語環境の完璧なセットアップ

2. [第1章：基礎編 - 高速起動と最小限のEmacs設定](chapters/02_foundation.md)
   - 起動時間1秒以内の実現
   - org-modeによる文芸的プログラミング
   - パフォーマンス測定の仕組み

3. [第2章：パッケージ管理編 - Elpacaによる拡張性の獲得](chapters/03_package.md)
   - 最新のパッケージマネージャー導入
   - use-packageによる宣言的な設定

### エディタ機能編
4. [第3章：エディタ基礎編 - 使いやすさの向上](chapters/04_editor.md)
   - 基本的な編集機能の強化
   - バックアップとundo機能の設定

5. [第4章：Evil編 - Vimの力を手に入れる](chapters/05_evil.md)
   - Vimキーバインドの導入
   - リーダーキーによる効率的な操作
   - 安定したundo/redo機能と視覚的な編集履歴

6. [第5章：補完システム編 - 賢い入力支援](chapters/06_completion.md)
   - 高速な補完システムの構築
   - ファジー検索の導入
   - VSCodeライクな操作の実現

### 開発環境編
7. [第6章：開発環境編 - LSPとGit統合](chapters/07_development.md)
   - 言語サーバー（LSP）の設定
   - Gitインテグレーション
   - プロジェクト管理

8. [第7章：ターミナル統合編 - シェルとの完全融合](chapters/08_terminal.md)
   - 完全なターミナルエミュレータの統合
   - Emacs内でのシェル作業の効率化

### カスタマイズ編
9. [第8章：UI/UX改善編 - 美しさと使いやすさの追求](chapters/09_ui.md)
   - Doomテーマの導入
   - モードラインのカスタマイズ
   - アイコンの追加

10. [第9章：言語別設定編 - 各言語への最適化](chapters/10_languages.md)
    - Go、Haskell、TypeScript、Python等の設定
    - 言語ごとのフォーマッタとリンタの統合

### 高度な機能編
11. [第10章：パフォーマンス最適化編 - 究極の高速化](chapters/11_performance.md)
    - 起動時間の最適化
    - メモリ使用量の削減
    - 遅延読み込みの戦略

12. [第11章：Org-mode活用編 - 最強のドキュメントシステム](chapters/12_orgmode.md)
    - GTDタスク管理
    - Org-roamによる知識管理
    - プレゼンテーション機能

13. [第12章：デバッグとテスト編 - 品質の追求](chapters/13_debug_test.md)
    - DAP-modeによるデバッグ
    - テストフレームワークの統合
    - TDD支援機能

## 🚀 クイックスタート

```bash
# リポジトリのクローン
git clone https://github.com/yuse-wy/vanilla-emacs-book.git
cd vanilla-emacs-book

# 章ごとに順番に読み進める
open chapters/00_introduction.md
```

## 📖 この本の特徴

- **段階的学習**: 各章は独立しており、必要な章から始められます
- **実践的**: すべての設定は実際に動作確認済み
- **詳細な説明**: 各設定の「なぜ」を説明
- **トラブルシューティング**: よくある問題と解決策を含む

## 🎯 目標

- ✅ 起動時間1秒以内
- ✅ VSCodeライクな操作性
- ✅ Vimの編集効率
- ✅ LSPによる高度な開発支援
- ✅ 美しく機能的なUI

## 📝 前提条件

- macOS（Emacs Mac Port推奨）
- 基本的なEmacsの知識
- プログラミング経験

## 🤝 貢献

Issues、Pull Requestsを歓迎します。

## 📜 ライセンス

MIT License

---

*このドキュメントは、Doom Emacsから独自のVanilla Emacs環境への移行を目指す方のためのガイドです。*