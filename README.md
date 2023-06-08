ペントミノソルバー
==================

![Pentomino Solver](ss.png)

#### Rust

```sh
$ cd rust
$ time cargo run --release -- --size=6
$ time cargo run --release -- --dlx --size=6
```

#### Haskell

```sh
$ cd haskell
$ time stack run -- --size=6
```


### 計測結果

| 実装方法 | 実行速度(秒) |
|:--------|-----------:|
| Rust (naive) | 0.29 |
| Haskell      | 1.32 |
| Rust (DLX)   | 4.27 |


### リンク

[ペントミノパズルを解く（深さ優先探索法、Dancing Links法） - Kludge Factory](https://tyfkda.github.io/blog/2023/05/12/pentomino-solver.html)
