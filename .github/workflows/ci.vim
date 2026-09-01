name: Haskell Script CI

on:
    push:
        branches: [ main ]
    pull_request:
        branches: [ main ]

permissions:
    contents: read

jobs:
    test:
        name: Run Script Test
        runs-on: ubuntu-latest

        steps:
        # 1. ソースコードのチェックアウト
        - name: Checkout repository
          uses: actions/checkout@v4

        # 2. Haskell 環境（Stack）のセットアップ
        - name: Setup Haskell
          uses: haskell-actions/setup@2
          with:
            enable-stack: true
            stack-no-global: true

        # 3. グローバル依存関係のキャッシュ（2回目以降の高速化）
        - name: Cache Stack global
          uses: actions/cache@v4
          with:
            path: ~/.stack
            key: ${{ runner.os }}-stack-global-${{ hashFiles('**/*.hs') }}
            restore-keys: |
              ${{ runner.os }}-stack-global-

        # 4. スクリプトのテストを実行
        - name: Run test script
          run: stack test.hs

