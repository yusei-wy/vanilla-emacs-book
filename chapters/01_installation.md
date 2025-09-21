# 第0章：準備編 - macOSへのEmacsインストール

### 🎯 この章の目標
- macOSに最適なEmacsをインストールする
- 日本語環境を完璧にセットアップする
- 今後のカスタマイズの土台を作る

### 📝 実装内容

#### Emacs Mac Port（EMP版）のインストール

##### なぜEMP版か？
- **日本語入力**: IMEのちらつき問題が解決済み、`(mac-auto-ascii-mode 1)`で自動切り替え
- **macOS統合**: スムーズスクロール、トラックパッドジェスチャー、Retinaディスプレイ対応
- **高パフォーマンス**: Apple Silicon（M1/M2/M3）での最適化

##### インストールオプションの選択

```shell
# 推奨：フル機能版（プログラミング + 文書作成）
brew tap railwaycat/emacsmacport
brew install emacs-mac \
  --with-native-comp \
  --with-imagemagick \
  --with-librsvg \
  --with-starter

# 最小限版（プログラミング中心）
brew install emacs-mac --with-native-comp

# vterm（高性能ターミナル）を使う場合の追加パッケージ
brew install cmake libvterm
```

##### 各オプションの効果
| オプション | 効果 | 必要度 |
|-----------|------|--------|
| `--with-native-comp` | Elispを高速化（2-5倍） | ⭐⭐⭐ 必須 |
| `--with-imagemagick` | PDF/画像表示 | ⭐⭐⭐ 推奨 |
| `--with-librsvg` | SVGアイコン表示 | ⭐⭐ 推奨 |
| `--with-xwidgets` | 内蔵Webブラウザ | ⭐ オプション |
| `--with-starter` | CLIからGUI起動 | ⭐⭐ 便利 |

##### インストール後の設定

```shell
# GUIアプリとして配置
cp -a /opt/homebrew/opt/emacs-mac/Emacs.app /Applications

# 動作確認
open /Applications/Emacs.app
emacs --version  # ターミナルから確認
```

### ⚠️ 注意事項
- **初回起動**: `--with-native-comp`使用時は、初回起動でパッケージコンパイルのため時間がかかる（5-10分）
- **ディスク容量**: ネイティブコンパイルは約1GB追加で使用
- **依存パッケージ**: ほとんどは自動インストールされるが、vtermのみ手動で`cmake`と`libvterm`が必要

### ✨ この章で得られたもの
- ✅ 日本語入力が完璧に動作するEmacs
- ✅ macOSとの深い統合（スムーズスクロール、ジェスチャー）
- ✅ ネイティブコンパイルによる高速動作
- ✅ 画像・PDF表示機能
- ✅ これから育てていく「種」としてのEmacs

---

[目次に戻る](00_introduction.md) | [次の章へ →](02_foundation.md)