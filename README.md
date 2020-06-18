# aheui-clj

난해한 한글 프로그래밍 언어 [아희](http://aheui.github.io/)의 Clojure 구현입니다.


### Examples

```clojure
(require '[aheui.core :as aheui])
=> nil
(with-out-str (aheui/run "밝밦따망희"))
=> "42"
```
