ペントミノソルバー
==================

![Pentomino Solver](ss.png)

#### Rust

```sh
$ cd rust
$ cargo run --release -- --size=6
$ cargo run --release -- --dlx --size=6
```

#### Haskell

```sh
$ cd haskell
$ stack run -- --size=6
```

#### Java

```sh
$ cd java
$ make
$ make run
$ java -cp dist PentominoSolver -s 8 --figure
```


### 計測結果

| 実装方法 | 実行速度(秒) |
|:--------|-----------:|
| Rust (naive) | 0.175 |
| Java         | 0.242 |
| Haskell      | 0.848 |
| Rust (DLX)   | 4.089 |


### 図形表示

```
$ cargo run --release -- -s3 --figure
┏━━━┳━┳━━━━━━━━━┳━━━━━┳━┳━┳━┳━━━━━━━┳━┳━┓
┃U┏━┛X┗━┳━━━┳━┳━┛N┏━━━┛F┃T┃W┗━┓Y┏━━━┛Z┃V┃
┃ ┗━┓ ┏━┛P  ┃L┗━━━┻━┓ ┏━┛ ┗━┓ ┗━┫ ┏━━━┛ ┃
┗━━━┻━┻━━━━━┻━━━━━━━┻━┻━━━━━┻━━━┻━┻━━━━━┛

┏━━━┳━┳━━━━━━━━━┳━┳━━━┳━━━━━┳━┳━━━━━━━┳━┓
┃U┏━┛X┗━┳━━━┳━━━┛Z┣━┓W┗━┓T┏━┛F┗━┳━━━┓L┃V┃
┃ ┗━┓ ┏━┛P  ┃ ┏━━━┛Y┗━┓ ┃ ┃ ┏━━━┛N┏━┻━┛ ┃
┗━━━┻━┻━━━━━┻━┻━━━━━━━┻━┻━┻━┻━━━━━┻━━━━━┛

Total: Solution=2, check=10793, elapsed=1.341917ms
```


### リンク

[ペントミノパズルを解く（深さ優先探索法、Dancing Links法） - Kludge Factory](https://tyfkda.github.io/blog/2023/05/12/pentomino-solver.html)
