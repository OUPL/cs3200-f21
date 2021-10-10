#| 0. Write your name and OU ID (the part before the
   "@" in your email address) below:

   NAME: Andriy Kalinichenko
   ID: ak461717
|#

# I highly recommend using this equality function instead of the '=='
# operator by itself.
fun eq<A>(x :: A, y :: A) -> Boolean:
  x == y
end

#| Result type code |#

# Specialized to String errors.
data Result<A>:
  | err(e :: String)
  | ok(r :: A)
end

# Map a function over a Result.
fun fmap<A, B>(f :: (A -> B), r :: Result<A>) -> Result<B>:
  cases (Result) r:
    | err(e) => err(e)
    | ok(a) => ok(f(a))
  end
end

# Flatten a Result of Result.
fun join<A>(r :: Result<Result<A>>) -> Result<A>:
  cases (Result) r:
    | err(e) => err(e)
    | ok(x) => x
  end
end

# Sequential composition.
fun seq<A, B>(r :: Result<A>, k :: (A -> Result<B>)) -> Result<B>:
  join(fmap(k, r))
end

# Parallel composition.
fun plus<A>(x :: Result<A>, y :: Result<A>) -> Result<A>:
  cases (Result) x:
    | err(_) => y
    | else => x
  end
end

# Parallel composition generalized to lists of Results.
fun choice<A>(l :: List<Result<A>>, e :: String) -> Result<A>:
  l.foldr(plus, err(e))
end

# Lift a function of two arguments into the Result type.
fun lift2<A, B, C>(f :: (A, B -> C)) -> (Result<A>, Result<B> -> Result<C>):
  lam(x, y):
    seq(x, lam(a):
        seq(y, lam(b):
            ok(f(a, b))
          end)
      end)
  end
end

# END Result type code


#| A binary /search/ tree (BST), in contrast to a regular old binary
   tree, is one in which:

   (i) The nodes -- not the leaves -- contain the data, and

   (ii) The data items are ordered such that for any node, the data
   items in the node's left child are less than the data item in the
   node and the data items in the node's right child are greater than
   the data item in the node.

   If you've forgotten how binary search trees work, you can refresh
   your memory here:
   https://en.wikipedia.org/wiki/Binary_search_tree.

   We'll be using BSTs of Numbers with the usual ordering on Numbers
   (as provided by Pyret's '<' operator).
|#

data BSTree:
  | bst-leaf
  | bst-node(val :: Number, left :: BSTree, right :: BSTree)
end

#| The binary search tree invariant can be encoded as a function in
   Pyret as follows: |#

fun bst-all(f :: (Number -> Boolean), t :: BSTree) -> Boolean:
  cases (BSTree) t:
    | bst-leaf => true
    | bst-node(n, l, r) => f(n) and bst-all(f, l) and bst-all(f, r)
  end
end

fun bst-inv(b :: BSTree) -> Boolean:
  cases(BSTree) b:
    | bst-leaf => true
    | bst-node(m, l, r) =>
      bst-inv(l) and
      bst-inv(r) and
      bst-all(lam(n): n < m end, l) and
      bst-all(lam(n): n > m end, r)
  end
end

#| The type of ordered binary search trees (elements of BSTree
   that satisfy bst-inv) |#
type BST = BSTree%(bst-inv)

# Trees used for testing below
b1 = bst-node(3, bst-leaf, bst-leaf)
b2 =
  bst-node(4,
    bst-node(1, bst-leaf, bst-leaf),
    bst-node(5, bst-leaf, bst-leaf))
b3 =
  bst-node(7,
    bst-node(4,
      bst-node(3, bst-leaf, bst-leaf),
      bst-node(5, bst-leaf, bst-leaf)),
    bst-node(9,
      bst-node(8, bst-leaf, bst-leaf),
      bst-node(10, bst-leaf, bst-leaf)))
b4 =
  bst-node(3,
    bst-node(3, bst-leaf, bst-leaf),
    bst-leaf)
b5 =
  bst-node(7,
    bst-node(4,
      bst-node(4, bst-leaf, bst-leaf),
      bst-node(5, bst-leaf, bst-leaf)),
    bst-node(9,
      bst-node(8, bst-leaf, bst-leaf),
      bst-node(10, bst-leaf, bst-leaf)))
# END Trees used for testing

check:
  bst-leaf satisfies bst-inv
  b1 satisfies bst-inv
  b2 satisfies bst-inv
  b3 satisfies bst-inv
  b4 violates bst-inv
  b5 violates bst-inv
end

#| 1. (3 pts) Define a function 'bst-lookup' that determines whether a
   given number is present in a binary search tree. You may assume
   that the BST passed as an argument to bst-lookup satisfies the
   binary search tree invariant as expressed in function bst-inv. |#

fun bst-lookup(n :: Number, t :: BST) -> Boolean:
  cases (BST) t:
    | bst-leaf => false
    | bst-node(m,l,r) => if eq(m,n) : true else: bst-lookup(n,l) or bst-lookup(n,r) end
  end
where:
  bst-lookup(3, bst-leaf) is false
  bst-lookup(2, b1) is false
  bst-lookup(3, b1) is true
  bst-lookup(1, b2) is true
  bst-lookup(4, b2) is true
  bst-lookup(6, b2) is false
  bst-lookup(5, b3) is true
  bst-lookup(8, b3) is true
  bst-lookup(10, b3) is true
  bst-lookup(11, b3) is false
end

#| 2. (3 pts) Define a function 'bst-insert' that inserts a number
   into a binary search tree such that the following property holds:

   if bst-inv(b) is true initially, then bst-inv(bst-insert(b, n)) is
   true after the insertion. I.e., bst-insert should preserve the BST
   invariant (note that this property is encoded into the type of
   the function by the use of BST instead of BSTree).

   NOTE: If the number is already in the tree, then your
   bst-insert function should leave the tree unchanged. |#

fun bst-insert(n :: Number, t :: BST) -> BST:
  cases (BST) t:
    | bst-leaf => bst-node(n, bst-leaf, bst-leaf)
    | bst-node(m,l,r) => if (eq(m,n)) : bst-node(m,l,r) 
      else if (n < m) : bst-insert(n,l)
      else: bst-insert(n,r)
          end
        end
where:
  bst-insert(3, bst-leaf) satisfies bst-inv
  bst-insert(1, b1) satisfies bst-inv
  bst-insert(3, b1) satisfies bst-inv
  bst-insert(2, b2) satisfies bst-inv
  bst-insert(4, b2) satisfies bst-inv
  bst-insert(9, b2) satisfies bst-inv
  bst-insert(22, b3) satisfies bst-inv
  bst-insert(7, b3) satisfies bst-inv
  bst-insert(-1, b3) satisfies bst-inv
  bst-insert(6, b3) satisfies bst-inv
end


#| The following data structure, RBTree, defines red-black binary
   trees (essentially just BTrees augmented with color information --
   red or black). If you haven't seen such trees before, or
   just forget, you can refresh your memory by reading the article
   here: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree. |#

data Color:
  | red
  | black
end

data RBTree<T>:
  | rbt-leaf(c :: Color)
  | rbt-node(c :: Color, val :: T, left :: RBTree<T>, right :: RBTree<T>)
end

#| 3. (10 pts) In order for an RBTree to be a valid red-black tree,
   all of the following properties must hold:

   (o) All nodes are either red or black.

   (i) The root node is black.

   (ii) All leaves are black.

   (iii) If a node is red, then both its children are black.

   (iv) Any path from any node in the tree to any of its descendent
   black leaf nodes has the same number of black nodes (not including
   the node itself but including the leaf node) as any other such path
   from that node.

   (v) The tree additionally satisfies the binary search tree
   invariant as explained in WARMUP exercise 3 above.

   These properties together imply that the height of an RBTree is
   pretty much balanced -- no left subtree of a node can be more than
   2x deeper than the right subtree of that node, and vice versa.

   Define a function 'rbt-inv' that determines whether a given RBTree
   satisfies the RBT invariant as described above.

   HINT 1: You probably don't need to directly encode property (o) --
   it's automatically true of every RBTree by construction.

   HINT 2: You may find it helpful to define one function for each
   property (i--iv) above. Then your overall rbt-inv function can just
   string these individual invariant-checking functions together. |#


# Some RBTrees used for testing:
rleaf = rbt-leaf(red)
bleaf = rbt-leaf(black)
r1 =
  rbt-node(black, 2,
    rbt-node(red, 1, bleaf, bleaf),
    rbt-node(red, 3, bleaf, bleaf))
r2 = rbt-node(red, 1, bleaf, bleaf)
r3 = rbt-node(black, 4,
  rbt-node(red, 2,
    rbt-node(black, 1, bleaf, bleaf),
    rbt-node(red, 3, bleaf, bleaf)),
  rbt-node(red, 6,
    rbt-node(black, 5, bleaf, bleaf),
    rbt-node(black, 7, bleaf, bleaf)))
r4 = rbt-node(black, 4,
  rbt-node(red, 2,
    rbt-node(black, 1, bleaf, bleaf),
    rbt-node(black, 3, bleaf, bleaf)),
  rbt-node(black, 6, bleaf, bleaf))
r5 = rbt-node(black, 4,
  rbt-node(red, 2,
    rbt-node(black, 1, bleaf, bleaf),
    rbt-node(black, 3, bleaf, bleaf)),
  rbt-node(red, 6,
    rbt-node(red, 5, bleaf, bleaf),
    rbt-node(black, 7, bleaf, bleaf)))
r6 = rbt-node(black, 4,
  rbt-node(red, 2,
    rbt-node(black, 1, bleaf, bleaf),
    rbt-node(black, 3, bleaf, bleaf)),
  rbt-node(red, 8,
    rbt-node(black, 6, bleaf, bleaf),
    rbt-node(black, 10, bleaf, bleaf)))
r7 = rbt-node(black, 4,
  rbt-node(red, 2,
    rbt-node(black, 1, bleaf, bleaf),
    rbt-node(black, 3, bleaf, bleaf)),
  rbt-node(black, 6,
    rbt-node(black, 5, bleaf, bleaf),
    rbt-node(black, 7, bleaf, bleaf)))
r8 = rbt-node(black, 1,
  rbt-node(red, 3, bleaf, bleaf),
  rbt-node(red, 2, bleaf, bleaf))
# END RBTrees used for testing

fun next-node(t :: RBTree<Number>) -> Color:
  cases (RBTree<Number>) t:
    | rbt-leaf(bl) => black
    | rbt-node(c,v,l,r) => if (c == red) : red else: black end
  end
end

fun leaf-blk(t :: RBTree<Number>) -> Boolean:
  cases (RBTree<Number>) t:
    | rbt-leaf(c) => if (c == black) : true else: false end
    | rbt-node(c,v,l,r) => leaf-blk(l) and leaf-blk(r)
  end
end

fun child-color(t :: RBTree<Number>) -> Boolean:
  cases (RBTree<Number>) t:
    | rbt-leaf(bl) => true
    | rbt-node(cl,v,l,r) => if (cl == red) : 
        if ((next-node(l) == black) and (next-node(r) == black)) : child-color(l) and child-color(r)
        else: false end
      else: if (cl == black) : child-color(l) and child-color(r) else: ... end end 
  end
end

fun even-blk(t :: RBTree<Number>) -> Number:
  cases (RBTree<Number>) t:
    | rbt-leaf(bl) => 1
    | rbt-node(cl,v,l,r) => if (cl == red) : 0 + even-blk(l) + even-blk(r) else: 1 + even-blk(l) + even-blk(r) end 
  end
end
fun rbt-inv(t :: RBTree<Number>) -> Boolean:
  cases (RBTree<Number>) t:
    | rbt-leaf(c) => if (c == bleaf) : true else: false end
    | rbt-node(c,v,l,r) => (c == black) and leaf-blk(t) and child-color(t) and ((even-blk(l) == even-blk(r)) and ((even-blk(l) + even-blk(r)) > 4))
  end
where:
  r1 satisfies rbt-inv
  r2 violates rbt-inv
  r3 violates rbt-inv
  r4 satisfies rbt-inv
  r5 violates rbt-inv
  r6 satisfies rbt-inv
  r7 violates rbt-inv
  r8 violates rbt-inv
end

# END red-black trees


#| Streams |#

data Stream<A>:
  | lz-link(h :: A, t :: ( -> Stream<A>))
end

# Helper for observing the value at the head of a stream.
fun lz-first<A>(s :: Stream<A>) -> A:
  cases (Stream) s:
    | lz-link(h, _) => h
  end
end

# Helper for observing the tail of a stream.
fun lz-rest<A>(s :: Stream<A>) -> Stream<A>:
  cases (Stream) s:
    | lz-link(_, t) => t()
  end
end

fun nats-from(n :: Number) -> Stream<Number>:
  lz-link(n, lam(): nats-from(n + 1) end)
end

# The stream of natural numbers.
nats = nats-from(0)

# Map a function over a stream.
fun lz-map<A, B>(f :: (A -> B), s :: Stream<A>) -> Stream<B>:
  lz-link(f(lz-first(s)), lam(): lz-map(f, lz-rest(s)) end)
end

# Take the nth finite approximation of a stream (a list containing the
# first n elements).
fun prefix<A>(n :: Number, s :: Stream<A>) -> List<A>:
  if n <= 0:
    empty
  else:
    link(lz-first(s), prefix(n - 1, lz-rest(s)))
  end
where:
  prefix(5, nats) is [list: 0, 1, 2, 3, 4]
  prefix(6, lz-map(lam(n): 2 * n end, nats)) is [list: 0, 2, 4, 6, 8, 10]
end


#| Approximating pi with streams.

   The following stream 'pi-sequence' generates a sequence of terms
   whose sum converges to π in the limit. I.e.,

   ∞
   ∑ pi-sequenceₖ = π
  k=0

   where pi-sequenceₖ is the kth element of pi-sequence.
|#

pi-sequence :: Stream<Number> = lz-map(lam(k):
    4 * (num-expt(-1, k) / ((2 * k) + 1))
  end, nats)

#| 4. (3 pts) Define a function 'stream-sums' that takes a
   Stream<Number> and produces the stream of its partial sums. That
   is, given a stream s, the nth element of stream-sums(s) should be
   the sum of all sₖ for k ≤ n. I.e.,

                     n   
   stream-sums(s)ₙ = ∑ sₖ
                    k=0
|#

fun stream-sums(s :: Stream<Number>) -> Stream<Number>:
  ... # Fill in here
where:
  prefix(5, stream-sums(nats)) is [list: 0, 1, 3, 6, 10]
  prefix(5, stream-sums(stream-sums(nats))) is [list: 0, 1, 4, 10, 20]
end

data Void:
end
fun print-stream<A>(s :: Stream<A>) -> Void:
  block:
    print(to-repr(lz-first(s)) + "\n")
    print-stream(lz-rest(s))
  end
end

# Uncomment the following line to print the entire stream (warning:
# doesn't terminate - hence the Void return type).
# print-stream(lz-map(num-to-roughnum, stream-sums(pi-sequence)))

#| 5. (3 pts) Define a function 'approximate' that takes a tolerance
   value and a stream and traverses the stream until subsequent values
   differ by less than the tolerance. That is, approximate(ε, s)
   should return sₙ where |sₙ₋₁ - sₙ| < ε.
|#

fun approximate(tolerance :: Number, s :: Stream<Number>) -> Number:
  ... # Fill in here
where:
  # The stream of partial sums of pi-sequence (converges to π).
  pi-series = stream-sums(pi-sequence)
  
  # A known approximation of π to test against.
  pi :: Number = 3.141592653589793
  
  approximate(0.1, pi-series) is%(num-within-abs(0.2)) pi
  approximate(0.1, pi-series) is-not%(num-within-abs(0.02)) pi
  approximate(0.01, pi-series) is%(num-within-abs(0.02)) pi
  approximate(0.01, pi-series) is-not%(num-within-abs(0.002)) pi
  # uncomment the following for extra assurance (might be a little slow)
  # approximate(0.001, pi-series) is%(num-within-abs(0.002)) pi
end


#| Error handling with the Result type. |#

data Exp:
  | Var(x :: String)
  | Num(n :: Number)
  | Plus(l :: Exp, r :: Exp) # l + r
  | Div(l :: Exp, r :: Exp) # l / r
end

# An environment is a function from identifiers (strings) to values.
type Env = (String -> Result<Number>)

# The initial environment contains no bindings.
init-env :: Env = lam(x :: String): err(x + " is unbound") end

# Look up the value bound to an identifier in an environment.
fun lookup(env :: Env, x :: String) -> Result<Number>:
  env(x)
end

#| 6. (3 pts) Rewrite the evaluator from PA2 using the Result type. |#

fun eval(env :: Env, e :: Exp) -> Result<Number>:
  ... # Fill in here
where:
  eval(init-env, Num(5)) is ok(5)
  eval(init-env, Plus(Num(5), Num(6))) is ok(11)
  eval(init-env, Plus(Num(10), Plus(Num(5), Num(6)))) is ok(21)
  eval(init-env, Div(Num(5), Num(6))) is ok(5 / 6)
  eval(init-env, Div(Num(1), Num(0))) is err("division by zero")
  eval(init-env, Div(Plus(Num(5), Num(6)), Plus(Num(5), Num(-5))))
    is err("division by zero")
  eval(init-env, Plus(Num(1), Div(Num(100), Plus(Num(5), Num(-5)))))
    is err("division by zero")

  # env1 = { x -> 3, y -> 4 }.
  env1 = lam(ident :: String):
    if eq(ident, "x"): ok(3)
    else if eq(ident, "y"): ok(4)
    else:
      err(ident + " is unbound")
    end
  end
  eval(env1, Num(5)) is ok(5)
  eval(init-env, Plus(Num(5), Var("x"))) is err("x is unbound")
  eval(env1, Plus(Num(5), Var("x"))) is ok(8)
  eval(init-env, Plus(Num(5), Var("y"))) is err("y is unbound")
  eval(env1, Plus(Num(5), Var("y"))) is ok(9)
  eval(env1, Plus(Var("x"), Var("x"))) is ok(6)
  eval(env1, Plus(Var("x"), Var("y"))) is ok(7)
  eval(env1, Div(Plus(Var("y"), Var("x")), Num(6))) is ok(7 / 6)
  eval(env1, Div(Plus(Var("y"), Var("x")), Var("z"))) is err("z is unbound")

  # env2 = { x -> 3, y -> 4, z -> 5 }.
  env2 = lam(ident :: String):
    if eq(ident, "x"): ok(3)
    else if eq(ident,"y"): ok(4)
    else if eq(ident, "z"): ok(5)
    else:
      err(ident + " is unbound")
    end
  end
  eval(env2, Num(5)) is ok(5)
  eval(env2, Div(Plus(Var("y"), Var("x")), Var("z"))) is ok(7 / 5)
  eval(env2, Div(Plus(Var("y"), Var("x")), Plus(Num(2), Var("z")))) is ok(1)
  eval(env2, Div(Plus(Var("y"), Var("x")), Plus(Var("w"), Var("z"))))
    is err("w is unbound")
end
