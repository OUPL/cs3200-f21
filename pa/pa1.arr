#| 0. Write your name and OU ID (the part before the 
   "@" in your email address) below:
  
   NAME:
   ID:
   
   Many of the exercises in this file have something to do with Pyret
   lists. I recommend -- before you get started -- that you peruse
   Pyret's list library documentation, available here:
   https://www.pyret.org/docs/latest/lists.html.
|#

#| PART I (Natural numbers):

   Recall that the natural numbers can be given by the following
   inductive definition:
   
   1) O (zero) is a natural number.
   
   2) If n is a natural number, then S(n) (the successor of n) is a
      natural number.
   
   3) The only natural numbers are those generated from 1) and 2).

   We can encode the natural numbers in Pyret with the
   following datatype (note that the zero constructor 'O' is spelled
   with a capital 'o'): |#

# The datatype of natural numbers.
data Nat:
  | O           # zero
  | S(n :: Nat) # successor
end

# Some Nats for testing.
one   = S(O)
two   = S(one)
three = S(two)
four  = S(three)
five  = S(four)
six   = S(five)

#| 1. (2 pts) As a warmup, define a function 'nat-plus' that takes two
   Nat arguments and produces their sum as a Nat (this should be easy
   if you attended lecture on 8/30 -- check the recording on YouTube
   if you forget). |#

fun nat-plus(n :: Nat, m :: Nat) -> Nat:
  ...
where:
  nat-plus(O, O) is O
  nat-plus(O, one) is one
  nat-plus(one, O) is one
  nat-plus(one, one) is two
  nat-plus(two, one) is three
  nat-plus(two, two) is four
  nat-plus(three, two) is five
end

#| 2. (2 pts) Using 'nat-plus' from #1, define a function 'nat-mult'
   that computes the product of two Nats (recall from 9/1 lecture). |#

fun nat-mult(n :: Nat, m :: Nat) -> Nat:
  ...
where:
  nat-mult(O, three) is O
  nat-mult(two, O) is O
  nat-mult(one, three) is three
  nat-mult(two, two) is four
  nat-mult(two, three) is six
end

#| 3. (2 pts) Using 'nat-mult' from #2, define a function 'nat-fact'
   that computes the factorial of a Nat (this one is not from lecture
   ;). |#

fun nat-fact(n :: Nat) -> Nat:
  ...
where:
  nat-fact(O) is one
  nat-fact(one) is one
  nat-fact(two) is two
  nat-fact(three) is six
  nat-fact(four) is nat-mult(six, four)
  nat-fact(five) is nat-mult(nat-mult(six, four), five)
end

#| PART II (Lists):
   
   4. (2 pts) As another warmup, define a function 'my-len' that
   computes the length of a list (producing a Nat result).
   
   NOTE: Don't use any library functions (like "len") in this
   exercise. Instead, uses "cases" and recursion to calculate the
   length yourself. |#

fun my-len<T>(l :: List<T>) -> Nat:
  ...
where:
  my-len([list: 0]) is one
  my-len([list: ]) is O
  my-len([list: 1, 2, 3, 4, 5]) is five
  my-len([list: 1, 2]) is two
  my-len([list: false]) is one
  my-len([list: [list: 1]]) is one
end

#| 5. (2 pts) Define a function 'my-append' that takes two List<T>
   arguments l1 and l2, for any type T, and returns the concatenation
   of the two lists. That is, your function should return a single
   lists that contains, first, all the elements of l1, then all the
   elements of the second list l2. |#

fun my-append<T>(l1 :: List<T>, l2 :: List<T>) -> List<T>:
  ...
where:
  my-append([list: ], [list: ]) is [list: ]
  my-append([list: ], [list: 1]) is [list: 1]
  my-append([list: 1], [list: ]) is [list: 1]
  my-append([list: 1, 2], [list: 3]) is [list: 1, 2, 3]
  my-append([list: [list: ]], [list: [list: ]]) is [list: [list:], [list:]]
end

#| 6. (2 pts) Define a function 'knil' that links a value of type T
   onto the *end* of a List<T>. For example: knil(1, [list: 3, 2])
   should result in [list: 3, 2, 1].

   HINT: Structure your program, as you presumably did in exercises 4
   and 5, as a case analysis on the input list l. Think recursively in
   the case when l is a link(f, r), with head or front element f and
   tail or rest r. What's a recursive way in which to move the new
   element "a" toward the end of the list, while maintaining the
   correct structure of the list up to that point? |#

fun knil<T>(a :: T, l :: List<T>) -> List<T>:
  ...
where:
  knil(1, [list: 3, 2]) is [list: 3, 2, 1]
  knil(0, [list: ]) is [list: 0]
  knil(false, [list: true, false]) is [list: true, false, false]
end

#| 7. (2 pts) Using your implementation of knil in #6, define
   a function 'my-rev' that reverses a list. For example,
   my-rev([list: 1, 2, 3]) should result in [list: 3, 2, 1]. |#

fun my-rev<T>(l :: List<T>) -> List<T>:
  ...
where:
  my-rev([list: 1, 2, 3]) is [list: 3, 2, 1]
  my-rev([list: ]) is [list: ]
  my-rev([list: 1]) is [list: 1]
  my-rev([list: false, true]) is [list: true, false]
end

#| 8. (2 pts) Using your my-append function from #5, define a function
   'my-flatten' that takes a list of lists and flattens it into a
   single list. For example, my-flatten([list: [list: 1], [list: ],
   [list: 2, 3]]) should result in the list [list: 1, 2, 3]. |#

fun my-flatten<T>(l :: List<List<T>>) -> List<T>:
  ...
where: 
  my-flatten([list: [list: 1], [list: ], [list: 2, 3]]) is [list: 1, 2, 3]
  my-flatten([list: [list: ]]) is [list: ]
  my-flatten([list: [list: [list: ]]]) is [list: [list: ]]
  my-flatten([list: [list: 1, 2, 3]]) is [list: 1, 2, 3]
  my-flatten([list: [list: false]]) is [list: false]
  my-flatten([list: [list: false], [list: ]]) is [list: false]
end

#| 9: (4 pts) Define a function, isort, that uses the insertion sort
   algorithm to sort a list of numbers, of type List<Number>. |#

fun isort(l :: List<Number>) -> List<Number>:
  ...
where: 
  isort([list: 3, 2, 1]) is [list: 1, 2, 3]
  isort([list:]) is [list:]
  isort([list: 1]) is [list: 1]
  isort([list: 3, 5, -7, 1, 0, 200]) is [list: -7, 0, 1, 3, 5, 200]
  # isort(my-rev(range(0, 100))) is range(0, 100)
end
