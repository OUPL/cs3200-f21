#| 0. Write your name and OU ID (the part before the
   "@" in your email address) below:

   NAME:
   ID:
|#

############################
#| Higher-order functions |#

#| 1. (2 pts) Define a function 'my-twice' that takes an
   arbitrary function f :: (T -> T) and an initial value a :: T and
   returns the result of applying f to a twice. |#

fun my-twice<T>(f :: (T -> T), a :: T) -> T:
  ...
where:
  my-twice(lam(x): x + 2 end, 3) is 7
  my-twice(lam(x): x end, false) is false
  my-twice(lam(x): my-twice(lam(y): y + 1 end, x) end, 0) is 4
end

#| 2. (2 pts) Define a function 'my-maptwice' that takes an
   arbitrary function f :: (T -> T) and a list of values of type T and
   returns the result of applying f twice to each value in the list. |#

fun my-maptwice<T>(f :: (T -> T), l :: List<T>) -> List<T>:
  ...
where:
  my-maptwice(lam(x :: Number): x + 2 end, [list: 1, 2, 3]) is [list: 5, 6, 7]
  my-maptwice(lam(x :: Number): x - 1 end, [list: ]) is [list: ]
  my-maptwice(lam(x :: Number): x - 1 end, [list: 0]) is [list: -2]
  my-maptwice(lam(x :: Number): my-twice(lam(y): y - 1 end, x) end, 
    [list: 1, 2, 3, 4, 5]) is [list: -3, -2, -1, 0, 1]
end

##################
#| Binary trees |#

#| In the next few exercises, you'll explore the following datatype,
   BTree<A>, which defines polymorphic binary search trees
   parameterized by the type A of data contained at the leaves. |#

data BTree<A>:
  | leaf(val :: A)
  | node(left :: BTree<A>, right :: BTree<A>)
end

#| 3. (2 pts) Referring to the definition of binary trees BTree<A>
   above, define a function 'num-leaves' that computes the number of
   leaves (NOTE: NOT the total number of nodes!) in a given BTree. |#

fun num-leaves<A>(b :: BTree<A>) -> Number:
  ...
where:
  num-leaves(leaf(3)) is 1
  num-leaves(node(leaf(3), leaf(4))) is 2
  num-leaves(node(node(leaf(3), leaf(4)), leaf(5))) is 3
  num-leaves(node(node(leaf(3), leaf(4)), node(leaf(3), leaf(4)))) is 4
end

#| 4. (2 pts) Define a function 'num-nodes' that computes the TOTAL
   number of nodes (link nodes and leaves) in a BTree. |#

fun num-nodes<A>(b :: BTree<A>) -> Number:
  ...
where:
  num-nodes(leaf(3)) is 1
  num-nodes(node(leaf(3), leaf(4))) is 3
  num-nodes(node(node(leaf(3), leaf(4)), leaf(5))) is 5
  num-nodes(node(node(leaf(3), leaf(4)), node(leaf(3), leaf(4)))) is 7
end

#| 5. (2 pts) Define a function 'contains' that evaluates to true iff
   a given BTree<Number> contains a particular number n. |#

fun contains(b :: BTree<Number>, n :: Number) -> Boolean:
  ...
where:
  contains(leaf(3), 2) is false
  contains(leaf(3), 3) is true
  contains(node(leaf(3), leaf(4)), 5) is false
  contains(node(leaf(3), leaf(4)), 4) is true
  contains(node(leaf(3), leaf(4)), 3) is true
  contains(node(node(leaf(3), leaf(4)), node(leaf(3), leaf(5))), 5) is true
  contains(node(node(leaf(3), leaf(4)), node(leaf(3), leaf(5))), 2) is false
end

#| 6. (2 pts) Define a function 'height' that computes the height of a
   BTree. Recall that the height of a *node* n is the longest number
   of edges from n to a leaf, and the height of a *tree* is the height
   of its root node. You may find the 'num-max' function from Pyret's
   Numbers library useful
   (https://www.pyret.org/docs/latest/numbers.html#%28part._numbers_num-max%29) |#

fun height<A>(b :: BTree<A>) -> Number:
  ...
where:
  height(leaf(5)) is 0
  height(node(leaf(3), leaf(4))) is 1
  height(node(leaf(3), node(leaf(4), leaf(5)))) is 2
  height(node(node(leaf(3), leaf(6)), node(leaf(4), leaf(5)))) is 2
  height(node(node(leaf(3), node(leaf(6), leaf(12))), node(leaf(4), leaf(5)))) is 3
end

#| 7. (2 pts) Define a function 'is-perfect' that evaluates to true
   iff a given BTree is a perfect binary tree. Recall that a binary
   tree is said to be perfect when all the leaf nodes have the same
   depth (# of edges from the root) and each internal node has two
   children (always true of our BTrees). Hint: you may find your
   'height' function from problem #4 to be useful. |#

fun is-perfect<A>(b :: BTree<A>) -> Boolean:
  ...
where:
  leaf(4) satisfies is-perfect
  node(leaf(4), node(leaf(3), leaf(5))) violates is-perfect
  node(node(leaf(4), leaf(4)), node(leaf(3), leaf(5))) satisfies is-perfect
  node(node(node(leaf(4), leaf(6)), leaf(4)), node(leaf(3), leaf(5))) violates is-perfect
  node(node(leaf(4), node(leaf(4), leaf(2))), node(leaf(3), leaf(5))) violates is-perfect
  node(node(leaf(4), leaf(4)), node(node(leaf(3), leaf(2)), leaf(5))) violates is-perfect
  node(node(leaf(4), leaf(4)), node(leaf(3), node(leaf(2), leaf(5)))) violates is-perfect
  node(node(leaf(4), leaf(4)), node(node(leaf(3), leaf(1)), node(leaf(2), leaf(5))))
    violates is-perfect
  node(node(node(leaf(0), leaf(4)), leaf(4)),
    node(node(leaf(3), leaf(1)), node(leaf(2), leaf(5)))) violates is-perfect
  node(node(node(leaf(0), leaf(4)), node(leaf(4), leaf(10))),
    node(node(leaf(3), leaf(1)), node(leaf(2), leaf(5)))) satisfies is-perfect
  node(node(node(leaf(2), leaf(3)), leaf(0)), node(leaf(2), node(leaf(1), leaf(0))))
    violates is-perfect
end

############################
#| Arithmetic expressions |#

#| Consider the following datatype for arithmetic expressions over
   Numbers with binary addition and division. |#

data Exp:
  | ENum(n :: Number)
  | EPlus(l :: Exp, r :: Exp) # l + r
  | EDiv(l :: Exp, r :: Exp) # l / r
end

#| 8. (3 pts) Define a function 'eval' that evaluates a given Exp to
   produce a Number. Your evaluator should raise a "division by zero"
   error in the event of division by zero.
|#

fun eval(e :: Exp) -> Number:
  ...
where:
  eval(ENum(5)) is 5
  eval(EPlus(ENum(5), ENum(6))) is 11
  eval(EPlus(ENum(10), EPlus(ENum(5), ENum(6)))) is 21
  eval(EDiv(ENum(5), ENum(6))) is 5 / 6
  eval(EDiv(ENum(5), EPlus(ENum(6), ENum(4)))) is 1 / 2
  eval(EDiv(ENum(1), ENum(0))) raises "division by zero"
  eval(EDiv(EPlus(ENum(5), ENum(6)), EPlus(ENum(5), ENum(-5)))) raises "division by zero"
  eval(EPlus(ENum(1), EDiv(ENum(100), EPlus(ENum(5), ENum(-5))))) raises "division by zero"
end

#| Now, consider a type of arithmetic expressions extended to
   include variables: |#

data VExp:
  | VVar(x :: String)
  | VNum(n :: Number)
  | VPlus(l :: VExp, r :: VExp) # l + r
  | VDiv(l :: VExp, r :: VExp) # l / r
end

#| To evaluate a VExp, we must provide an assignment of values
   (Numbers) to each of the variables that could appear in the
   expression. For this, we introduce a type of environments mapping
   variable names to their corresponding Number values. |#

# An environment is a function from identifiers (strings) to values.
type Env = (String -> Number)

# The initial environment contains no bindings.
init-env :: Env = lam(x :: String): raise(x + " is unbound") end

# Look up the value bound to an identifier in an environment.
fun lookup(env :: Env, x :: String) -> Number:
  env(x)
end

#| 9. (3 pts) Define a function 'veval' that evaluates a given VExp
   under a given environment. Your evaluator should raise a "division
   by zero" error in the event of division by zero.
|#

fun veval(env :: Env, e :: VExp) -> Number:
  ...
where:
  # Previous tests should still pass under the empty environment.
  veval(init-env, VNum(5)) is 5
  veval(init-env, VPlus(VNum(5), VNum(6))) is 11
  veval(init-env, VPlus(VNum(10), VPlus(VNum(5), VNum(6)))) is 21
  veval(init-env, VDiv(VNum(5), VNum(6))) is 5 / 6
  veval(init-env, VDiv(VNum(1), VNum(0))) raises "division by zero"
  veval(init-env, VDiv(VPlus(VNum(5), VNum(6)), VPlus(VNum(5), VNum(-5))))
    raises "division by zero"
  veval(init-env, VPlus(VNum(1), VDiv(VNum(100), VPlus(VNum(5), VNum(-5)))))
    raises "division by zero"

  # env1 = { x -> 3, y -> 4 }.
  env1 = lam(ident :: String):
    if ident == "x": 3
    else if ident == "y": 4
    else:
      raise(ident + " is unbound")
    end
  end
  veval(env1, VNum(5)) is 5
  veval(init-env, VPlus(VNum(5), VVar("x"))) raises "x is unbound"
  veval(env1, VPlus(VNum(5), VVar("x"))) is 8
  veval(init-env, VPlus(VNum(5), VVar("y"))) raises "y is unbound"
  veval(env1, VPlus(VNum(5), VVar("y"))) is 9
  veval(env1, VPlus(VVar("x"), VVar("x"))) is 6
  veval(env1, VPlus(VVar("x"), VVar("y"))) is 7
  veval(env1, VDiv(VPlus(VVar("y"), VVar("x")), VNum(6))) is 7 / 6
  veval(env1, VDiv(VPlus(VVar("y"), VVar("x")), VVar("z"))) raises "z is unbound"

  # env2 = { x -> 3, y -> 4, z -> 5 }.
  env2 = lam(ident :: String):
    if ident == "x": 3
    else if ident == "y": 4
    else if ident == "z": 5
    else:
      raise(ident + " is unbound")
    end
  end
  veval(env2, VNum(5)) is 5
  veval(env2, VDiv(VPlus(VVar("y"), VVar("x")), VVar("z"))) is 7 / 5
  veval(env2, VDiv(VPlus(VVar("y"), VVar("x")), VPlus(VNum(2), VVar("z")))) is 1
  veval(env2, VDiv(VPlus(VVar("y"), VVar("x")), VPlus(VVar("w"), VVar("z"))))
    raises "w is unbound"
end

#| 10. (OPTIONAL, EXTRA CREDIT) Some expressions contain parts that
   can be seen to be redundant by a static analysis phase (i.e., by
   just looking at the expression without having to evaluate it). For
   example, the expression 'VPlus(VNum(0), VVar(x))' will always
   evaluate to the same Number as simply 'VVar(x)', regardless of the
   value assigned to x by the environment.

   Write an optimizer function called 'opt' that takes a VExp and
   produces an optimized version in which redundant expressions have
   been eliminated. Try to think about what optimizations are
   possible, perhaps by appealing to algebraic identities (e.g., 0 + x
   = x as described above). Write your own test cases to check that
   your optimization pass is sound -- that is, that it doesn't change
   the meaning (the result of evaluation) of the VExp being optimized.
|#

# fun opt(e :: VExp) -> VExp:
#   ...
# end
