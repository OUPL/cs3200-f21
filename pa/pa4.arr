import s-exp as S

#| 0. Write your name and OU ID (the part before the
   "@" in your email address) below:

   NAME:
   ID:
|#

#########################
#| CS3200 LIBRARY CODE |#

# I highly recommend using this equality function instead of the '=='
# operator by itself.
fun eq<A>(x :: A, y :: A) -> Boolean:
  x == y
end

#| Result type code |#

data Error:
  | ParseError(msg :: String)
  | InterpError(msg :: String)
end

# Specialized to the Error type
data Result<A>:
  | err(e :: Error)
  | ok(r :: A)
end

fun is-parse-error<A>(r :: Result<A>) -> Boolean:
  cases (Result) r:
    | err(e) => is-ParseError(e)
    | else => false
  end
end

fun is-interp-error<A>(r :: Result<A>) -> Boolean:
  cases (Result) r:
    | err(e) => is-InterpError(e)
    | else => false
  end
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

fun seq2<A, B, C>(x :: Result<A>, y :: Result<B>, k :: (A, B -> Result<C>)) -> Result<C>:
  seq(x, lam(a): seq(y, lam(b): k(a, b) end) end)
end

# Parallel composition.
fun plus<A>(x :: Result<A>, y :: Result<A>) -> Result<A>:
  cases (Result) x:
    | err(_) => y
    | else => x
  end
end

# Parallel composition generalized to lists of Results.
fun choice<A>(l :: List<Result<A>>, e :: Error) -> Result<A>:
  l.foldr(plus, err(e))
end

# Lift a function of two arguments to the Result type.
fun lift2<A, B, C>(f :: (A, B -> C)) -> (Result<A>, Result<B> -> Result<C>):
  lam(x, y): seq2(x, y, lam(a, b): ok(f(a, b)) end) end
end

# END Result type code

#| END CS3200 LIBRARY CODE |#
#############################


#| In this assignment, you'll be implementing a parser for a small
   programming language called Scheme0 Core. The syntax of this
   language follows "S-expression" form, and is given by the following
   BNF grammar:

   Unary Operators
   u ::= not           # negate a boolean

   Binary Operators
   b ::= +             # add two numbers
       | -             # subtract two numbers
       | *             # multiply two numbers
       | /             # divide two numbers
       | =             # equality on numbers
       | <             # less-than on numbers

   Values:
   v ::= true          # boolean true
       | false         # boolean false
       | n             # a number

   Expressions:
   e ::= v               # a value
       | x               # an identifier
       | (u e)           # unary op u applied to expression e
       | (b e1 e2)       # binary op b applied to e1, e2
       | (if e1 e2 e3)   # if e1 then e2 else e3
       | (let x e2 e3)   # let x = the value of e2 in e3 (in which x may appear free)

   For example, here's a valid Scheme0 Core program, in concrete syntax:

     (let x 3
         (let y 4
             (+ x y)))

   As you might expect, this program evaluates to 7.
|#


#| (10 pts) Part 1: Scheme0 Core Parser
   
   In part 1, your job is to write a program called 'parseExp' that
   translates Pyret S-expressions into the Scheme0 Core abstract
   syntax given below. That is, we'll leave the job of actually
   parsing the concrete S-expression syntax up to Pyret (which has
   built-in libraries for handling this sort of thing), and instead
   focus just on the translation of S-expressions into Scheme0 Core's
   abstract syntax.

   This exercise is nontrivial (meaning it may take you
   while!). Here's how I suggest you get started:

   - First, read through the Scheme0 Core abstract syntax given
     below. Make sure you understand how the abstract syntax (as
     encoded by the algebraic datatypes for Exp, Val, etc.)
     corresponds to the Scheme0 Core BNF given above.

   - Second, read the provided test cases given at the end of the
     parseExp function.

   - Third, looking at the provided template code, figure out how you
     would like to attack this problem.

   - Fourth and finally, begin programming parseExp.
|#

#| Scheme0 Core Abstract Syntax |#

data Unop:
  | unot
end

data Binop:
  | add
  | sub
  | mul
  | div
  | equ
  | lt
end

data Val:
  | bool(b :: Boolean)
  | num(n :: Number)
end

data Exp:
  | val(v :: Val)
  | ident(s :: String)
  | unexp(op :: Unop, e :: Exp)
  | binexp(op :: Binop, e1 :: Exp, e2 :: Exp)
  | ite(e1 :: Exp, e2 :: Exp, e3 :: Exp)
  | letx(x :: String, e1 :: Exp, e2 :: Exp)
end

#| END Scheme0 Core Abstract Syntax |#

# Example expressions
ex1 = val(num(3))
ex2 = val(num(100))
ex3 = val(bool(true))
ex4 = val(bool(false))
ex5 = binexp(add, ex1, ex2)
ex6 = binexp(sub, ex1, ex2)
ex7 = binexp(mul, ex1, ex2)
ex8 = binexp(div, ex1, ex2)
ex9 = binexp(mul, ex8, ex8)
ex10 = binexp(mul, ex7, ex5)
ex11 = binexp(add, ident("x"), ident("y"))
ex12 = binexp(sub, val(num(0)), val(num(5)))
ex20 = unexp(unot, val(bool(true)))
ex30 = letx("x", val(num(3)), ident("x"))
ex31 = letx("yzw", binexp(add, val(num(4)), ident("x")), ident("x"))
ex32 = letx("x", binexp(add, val(num(4)), binexp(add, ident("x"), ident("x"))),
  binexp(mul, ident("x"), ident("y")))
ex33 = ite(val(bool(true)), val(num(5)), ident("x"))
ex34 = ite(val(bool(false)), val(num(5)), ident("x"))
ex35 = letx("x", val(num(6)), ite(val(bool(false)), val(num(5)), ident("x")))
ex40 = letx("b",
  ite(binexp(lt, val(num(1)), val(num(1))), val(num(4)), val(num(5))),
  binexp(sub, ident("b"), val(num(9))))
ex41 = letx("q", binexp(equ, val(num(3)), val(num(3))), ident("q"))


# Type synonym for the s-expression type.
type Sexp = S.S-Exp

# A few helpers that you may find useful.
fun as-sym<A>(s :: Sexp, k :: (String -> Result<A>)) -> Result<A>:
  cases (Sexp) s:
    | s-sym(x) => k(x)
    | else => err(ParseError("expected symbol, got " + to-repr(s)))
  end
end

fun as-list<A>(s :: Sexp, k :: (List<Sexp> -> Result<A>)) -> Result<A>:
  cases (Sexp) s:
    | s-list(l) => k(l)
    | else => err(ParseError("expected list, got " + to-repr(s)))
  end
end

# Parse a Unop.
fun parseUnop(s :: Sexp) -> Result<Unop>:
  as-sym(s, lam(sym):
      if sym == "not":
        ok(unot)
      else:
        err(ParseError("parseUnop"))
      end
    end)
end

# Parse a number value.
fun parseNum(s :: Sexp) -> Result<Val>:
  ... # Fill in here
end

# Parse a value.
fun parseVal(s :: Sexp) -> Result<Val>:
  choice([list:
      parseNum(s),
      ... # Fill in here
    ], ParseError("parseVal"))
end

# Parse an identifier.
fun parseIdent(s :: Sexp) -> Result<String>:
  ... # Fill in here
end

# Parse a unary expression.
fun parseUnexp(s :: Sexp) -> Result<Exp>:
  ... # Fill in here
end

# Convert an s-expression to a Scheme0 Core expression.
fun parseExp(s :: Sexp) -> Result<Exp>:
  choice([list:
      fmap(val, parseVal(s)),
      fmap(ident, parseIdent(s)),
      parseUnexp(s),
      ... # Fill in here
    ], ParseError("parseExp"))
end

# The overall parser is the composition of parseExp with S.read-s-exp.
fun parse(s :: String) -> Result<Exp>:
  parseExp(S.read-s-exp(s))
where:
  # values
  parse("3") is ok(ex1)
  parse("   3") is ok(ex1)
  parse("3   ") is ok(ex1)
  parse("100") is ok(ex2)
  parse("   100   ") is ok(ex2)
  parse("true") is ok(ex3)
  parse("   true") is ok(ex3)
  parse("false   ") is ok(ex4)
  parse("1 2") raises "Invalid s-expression"

  # binexps
  parse("(+ 3 100)") is ok(ex5)
  parse("(- 3 100)") is ok(ex6)
  parse("(* 3 100)") is ok(ex7)
  parse("(    *   3   100   )") is ok(ex7)
  parse("(/ 3 100)") is ok(ex8)
  parse("(* (/ 3 100) (/ 3 100))") is ok(ex9)
  parse("(* (* 3 100) (+ 3 100))") is ok(ex10)
  parse("(+ x y)") is ok(ex11)
  parse("(- 0 5)") is ok(ex12)

  # unexps
  parse("(not true)") is ok(ex20)
  parse("(not 1 2)") satisfies is-parse-error

  # let expressions
  parse("(let x 3 x)") is ok(ex30)
  parse("( let  yzw (+ 4 x) x  )") is ok(ex31)
  parse("(let x (+ 4 (+ x x)) (* x y))") is ok(ex32)
  parse("  ( let x (+ 4 (+ x x)) (*x  y  ) )") satisfies is-parse-error
  parse("(let x 3 x x)") satisfies is-parse-error
  parse("(let b (if (< 1 1) 4 5) (- b 9))") is ok(ex40)
  parse("(let q (= 3 3) q)") is ok(ex41)

  # if expressions
  parse("(if true 5 x)") is ok(ex33)
  parse("(if false 5 x)") is ok(ex34)
  parse("(let x 6 (if false 5 x))") is ok(ex35)
  parse("(cons false 5 x)") satisfies is-parse-error
  parse("(if false 5 x y)") satisfies is-parse-error
end


#| (10 pts) Part 2: Scheme0 Core Interpreter

   In this part, you will implement an interpreter for the Scheme0
   Core language.
   
   That is, define a function `interp` that takes an initial
   environment (mapping identifiers to values) and an expression, and
   returns the result of evaluating that expression to a value. As in
   the parser assignment, you may find it helpful to break the problem
   down into smaller functions. For example, you might define
   one function for interpreting binary expressions, one for
   conditional expressions, and so on.
|#

# An environment is a function from identifiers (strings) to values.
type Env = (String -> Result<Val>)

# The initial environment contains no bindings.
init-env :: Env = lam(x :: String): err(InterpError(x + " is unbound")) end

# Look up the value bound to an identifier in an environment.
fun lookup(rho :: Env, x :: String) -> Result<Val>:
  rho(x)
end

# Update an environment with a new binding.
fun upd(rho :: Env, x :: String, new-val :: Val) -> Env:
  lam(y): if eq(x, y): ok(new-val) else: rho(y) end end
end

# Evaluate an expression under a given environment, producing a value.
fun interp(rho :: Env, e :: Exp) -> Result<Val>:
  ... # Fill in here
end


# End-to-end parser and interpreter.
fun run(s :: String) -> Result<Val>:
  seq(parse(s), lam(e): interp(init-env, e) end)
end

# These tests provide evidence that your interpreter is working properly.
check "interp(...)":
  interp(init-env, ex1) is ok(num(3))
  interp(init-env, ex2) is ok(num(100))
  interp(init-env, ex3) is ok(bool(true))
  interp(init-env, ex4) is ok(bool(false))
  interp(init-env, ex5) is ok(num(103))
  interp(init-env, ex6) is ok(num(-97))
  interp(init-env, ex7) is ok(num(300))
  interp(init-env, ex8) is ok(num(0.03))
  interp(init-env, ex9) is ok(num(0.0009))
  interp(init-env, ex10) is ok(num(30900))
  interp(init-env, ex11) satisfies is-interp-error
  interp(init-env, ex12) is ok(num(-5))
  interp(init-env, ex20) is ok(bool(false))
  interp(init-env, ex30) is ok(num(3))
  interp(init-env, ex31) satisfies is-interp-error
  interp(init-env, ex32) satisfies is-interp-error
  interp(init-env, ex33) is ok(num(5))
  interp(init-env, ex34) satisfies is-interp-error
  interp(init-env, ex35) is ok(num(6))
  interp(init-env, ex40) is ok(num(-4))
  interp(init-env, ex41) is ok(bool(true))
end

# These tests provide evidence that your parser and interpreter are
# both working properly.
check "run(...)":
  run("3") is ok(num(3))
  run("true") is ok(bool(true))
  run("(* 3 4)") is ok(num(12))
  run("(* 3 true)") satisfies is-interp-error # boolean argument to multiplication
  run("(let x 3 4)") is ok(num(4))
  run("(let x 3 (* x true))") satisfies is-interp-error # boolean argument to multiplication
  run("(4 * 5)") satisfies is-parse-error
  run("(* true false)") satisfies is-interp-error # boolean argument to multiplication
  run("(let x 5 (* x x))") is ok(num(25))
  run("(LET x 5 (* x x))") satisfies is-parse-error
  run("(let x 4 (let x 5 (* x x)))") is ok(num(25))
  run("(+ (let x 0 (+ x x)) 0)") is ok(num(0))
  run("(/ (let x 0 0) (let x 0 x))") satisfies is-interp-error # division by zero
  run("(let x (let x (+ 3 4) x) (* x x))") is ok(num(49))
  run("(* x 4)") satisfies is-interp-error # unbound variable
  run("(if true 0 true)") is ok(num(0))
  run("(if false 0 true)") is ok(bool(true))
  run("(/ 1 2)") is ok(num(1/2))
  run("(- 0 (/ 1 2))") is ok(num(-1/2))
  run("(not 123)") satisfies is-interp-error # number argument to negation
  run("(let x 2 (let y 3 (let z 5 (- x (* y z)))))") is ok(num(-13))
  run("(if 0 1 2)") satisfies is-interp-error # non-boolean discriminee
  run("(+ 123 false)") satisfies is-interp-error # boolean argument to addition
  # Add your own test cases here.
end
