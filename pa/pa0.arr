#| 0. Write your name and OU ID (the part before the
   "@" in your email address) below:
  
   NAME: 
   ID:
|#

#| 1. (3 pts) Define a function 'eucl-dist' that returns the Euclidean
   distance between two points (x1,y1) and (x2,y2). Look up Euclidean
   distance on Wikipedia if you forget how it's calculated. A Pyret
   library that may be useful here is:
   
   -[Numbers](https://www.pyret.org/docs/latest/numbers.html)
   
   Your function should satisfy the tests defined in the [check] block
   below. Note the use of "is-roughly" and "is%(within-abs(...))",
   which we use here because your eucl-dist function should return a
   Pyret Roughnum rather than an Exactnum (you'll likely be
   using functions like num-sqrt and num-sqr -- recall from the
   documentation in the Numbers library linked above that Pyret
   Roughnums correspond to floats or doubles in a language like C++,
   and therefore can't safely be compared for exact equality). |#

fun eucl-dist(x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number) -> Number:
  ... # FILL IN HERE (replace the '...')
where:
  eucl-dist(0, 0, 0, 0) is-roughly 0
  eucl-dist(1, 1, 1, 1) is-roughly 0
  eucl-dist(0, 0, 1, 1) is-roughly num-sqrt(2)
  eucl-dist(1, 1, 0, 0) is-roughly num-sqrt(2)
  eucl-dist(-1, -2, -3, -4) is%(within-abs(0.001)) ~2.828
  eucl-dist(-3, -4, -1, -2) is%(within-abs(0.001)) ~2.828
end

#| 2. (4 pts) Define two functions, 'bool-and' and 'bool-or', that
   return, respectively, the Boolean conjunction or disjunction of
   their Boolean arguments b1 and b2.

   Pyret includes builtin infix functions "and" and "or" -- use these
   only as a last resort (instead, try to encode conjunction and
   disjunction using just "if/else/end" and the Boolean literals
   "true" and "false". |#

fun bool-and(b1 :: Boolean, b2 :: Boolean) -> Boolean:
  ... # FILL IN HERE
where:
  bool-and(false, false) is false
  bool-and(false, true) is false
  bool-and(true, false) is false
  bool-and(true, true) is true
end

fun bool-or(b1 :: Boolean, b2 :: Boolean) -> Boolean:
  ... # FILL IN HERE
where:
  bool-or(false, false) is false
  bool-or(false, true) is true
  bool-or(true, false) is true
  bool-or(true, true) is true
end

#| 3. (3 pts) Using 'bool-and' and 'bool-or' as defined in exercise 3,
   define a function, maj, that returns the majority result of three
   boolean values b1, b2, and b3. For example, if b1 and b2 are true
   while b3 is false, maj should return true.
   
   As in exercises 1 and 2, your function should pass all the
   test cases in the check block. |#

fun maj(b1 :: Boolean, b2 :: Boolean, b3 :: Boolean) -> Boolean:
  ... # FILL IN HERE
where:
  maj(false, false, false) is false
  maj(false, false, true) is false
  maj(false, true, false) is false
  maj(false, true, true) is true
  maj(true, false, false) is false
  maj(true, false, true) is true
  maj(true, true, false) is true
  maj(true, true, true) is true
end

#| 4. (2 pts) Modify the following function, 'hello-world', so that it
   passes its test case. Note that there are multiple correct
   solutions to this exercise. |#

fun hello-world() -> String:
  s1 = "hello"
  s2 = "world :-(" 
  s1 + s2 # recall that "+" is string concatenation
where:
  hello-world() is "hello\n" + " " + "\"world\"" + "!! :-)"
end

#| 5. (3 pts) Define a function 'fib' that computes the nth fibonacci
   number. Recall that the fibonacci sequence can be defined by the
   following set of (recursive ;)) equations:

   fib(0) = 0
   fib(1) = 1
   fib(n) = fib(n - 2) + fib(n - 1), for n >= 2

   'NumNonNegative' is the type of nonnegative Numbers. See
   https://www.pyret.org/docs/latest/numbers.html to read about
   Numbers in Pyret.
|#

fun fib(n :: NumNonNegative) -> NumNonNegative:
  ... # FILL IN HERE
where:
  range(0, 13).map(fib) is [list: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
end
