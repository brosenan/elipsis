(ns elipsis.core-test
  (:require [midje.sweet :refer :all]
            [elipsis.core :refer :all]))

(fact
 (deconstruct [] []) => {}
 (deconstruct '() []) => nil
 (deconstruct '(foo [bar]) '(1 [2])) => {'foo 1
                                         'bar 2}
 (deconstruct '(foo bar) '(1)) => nil
 (deconstruct '(foo) '(1 2)) => nil
 (deconstruct '(1 two 3) '(1 2 3)) => {'two 2}
 (deconstruct '(1 two 3) '(1 2 4)) => nil
 (deconstruct '(foo ...) '(1 2 3)) => {'foo [1 2 3]}
 (deconstruct '([foo bar] ...) '([1 2] [3 4])) => {'foo [1 3]
                                                   'bar [2 4]})


(fact
 (reconstruct [] {}) => []
 (reconstruct 'foo {'foo 3}) => 3
 (reconstruct 'foo {}) => 'foo
 (reconstruct '(foo bar) {'foo 1
                          'bar 2}) => '(1 2)
 (reconstruct [1 2] {}) => vector?
 (reconstruct '[foo ...] {'foo [1 2 3]}) => [1 2 3]
 (reconstruct '([foo bar] ...) {'foo [1 2 3]
                                'bar [4 5 6]}) => '([1 4] [2 5] [3 6]))

(fact
 (common-length 'foo {'foo [1 2 3]}) => 3
 (common-length 'foo {'foo [1 2]}) => 2
 (common-length '(foo bar) {'foo [1 2]
                            'bar [3 4]}) => 2
 (common-length '(foo bar) {'foo [1 2]
                            'bar [3]}) => (throws "Trying to reconstruct patterns of different lengths: (foo bar)"))
