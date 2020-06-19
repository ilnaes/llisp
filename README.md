# llisp

A lisp-like language written in Rust.  WIP.

A program like

```
(defn add (x)
    (lambda (y)
        (+ x y)))

(defn our_main ()
    (let ((f (add 1)))
        (f 2)))
```

gets compiled to

```llvm
declare void @print(i64)
declare i64* @new(i64)

define i64 @sym1(i64 %y, i64 %self) {
  %sym2 = inttoptr i64 %self to i64*
  %sym3 = getelementptr inbounds i64, i64* %sym2, i64 1
  %sym4 = load i64, i64* %sym3, align 8
  %sym5 = add i64 %sym4, %y
  %sym6 = sub i64 %sym5, 1
  ret i64 %sym6
}
define i64 @add(i64 %x, i64 %self) {
  %sym7 = call i64* @new(i64 3)
  %sym8 = ptrtoint i64(i64, i64)* @sym1 to i64
  store i64 %sym8, i64* %sym7, align 8
  %sym9 = ptrtoint i64* %sym7 to i64
  %sym10 = getelementptr inbounds i64, i64* %sym7, i64 1
  store i64 %x, i64* %sym10, align 8
  ret i64 %sym9
}
define i64 @our_main() {
  %sym11 = call i64* @new(i64 2)
  %sym12 = ptrtoint i64(i64, i64)* @add to i64
  store i64 %sym12, i64* %sym11, align 8
  %sym13 = ptrtoint i64* %sym11 to i64
  %sym18 = add i64 0, 3
  %sym14 = inttoptr i64 %sym13 to i64*
  %sym15 = load i64, i64* %sym14, align 8
  %sym16 = inttoptr i64 %sym15 to i64(i64, i64)*
  %sym17 = call i64 %sym16(i64 %sym18, i64 %sym13)
  %sym23 = add i64 0, 5
  %sym19 = inttoptr i64 %sym17 to i64*
  %sym20 = load i64, i64* %sym19, align 8
  %sym21 = inttoptr i64 %sym20 to i64(i64, i64)*
  %sym22 = call i64 %sym21(i64 %sym23, i64 %sym17)
  ret i64 %sym22
}
```
(_n.b._ integer values are tagged so their representations are different in the compiled version)

Supported + Wish list:

- [x] Let bindings
- [x] Basic primitive operations
- [x] Functions and function calls
- [x] Type inference
- [x] Closures
- [ ] Lists
- [ ] Strings
- [ ] Macros
- [ ] Memory management
