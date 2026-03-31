#lang sicp

#|
Q. 導出方法

(unless condition usual-value exceptional-value)

↓

(if condition
  exceptional-value
  usual-value)

Q. 手続きとして使えると有用である状況の例を述べよ

(map unless ...) が使える

|#

