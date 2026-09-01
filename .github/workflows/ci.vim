name: Haskell CI

on:
    push:
        branches: [ "main" ]
    pull_request:
        branches: [ "main" ]

" permissions:
"     contents: read

jobs:
    build-and-test:
        runs-on: ubuntu-latest

        steps:
            - name: Checkout repository
              uses: actions/checkout@v4

            - name: Setup Haskell Stack
              uses: haskell-actions/setup@v2
              with:
                ghc-version: '9.8.1'
                enable-stack: true
                stack-version: 'latest'

            - name: Cache ~/.stack and .stack-work
              uses: actions/cache@v4
              with:
                path: |
                  ~/.stack
                  .stack-work
                key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml.lock', 'package.yaml') }}
                restore-keys: |
                  ${{ runner.os }}-stack-
            - name: Build dependencies & project
              run: stack build --test --no-run-tests

            - name: Run Tasty Test Suite
              run: stack test
