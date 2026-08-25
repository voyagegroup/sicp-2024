#lang sicp

; Ben BitdiddleはFibonacci計算機の制御列にはそれを除去すると速くなり得る余分なsaveと余分なrestoreがあると見た. その命令はどれか.

afterfib-n-1
  (restore continue)
  (save continue)

; スタックから取り出したものを、そのままいれている