import s-exp as S

#| 0. Write your name and OU ID (the part before the
   "@" in your email address) below:

   NAME:
   ID:
|#

#########################
#| CS3200 LIBRARY CODE |#

fun curry<A, B, C>(f :: (A, B -> C)) -> (A -> (B -> C)):
  lam(a): lam(b): f(a, b) end end
end

fun eq<A>(x :: A, y :: A) -> Boolean:
  x == y
end
eqc = curry(eq)

fun compose<A, B, C>(g :: (B -> C), f :: (A -> B)) -> (A -> C):
  lam(a): g(f(a)) end
end

fun thunk<A>(x :: A) -> ( -> A):
  lam(): x end
end

fun constant<A, B>(b :: B) -> (A -> B):
  lam(_): b end
end

#| Result type code |#

data Error:
  | ParseError(msg :: String)
  | TypeError(msg :: String)
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

fun is-type-error<A>(r :: Result<A>) -> Boolean:
  cases (Result) r:
    | err(e) => is-TypeError(e)
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


#| In this assignment, you'll be extending the parser from PA4 to
   support new concrete syntax forms (that are desugared to Scheme0
   Core abstract syntax), and implementing a typechecker for Scheme0
   Core. The new concrete syntax is given by the following BNF grammar:

   Types
   t ::= tnum
       | tbool

   Unary Operators
   u ::= not           # negate a boolean
       | neg           # negate a number (NEW)

   Binary Operators
   b ::= +             # add two numbers
       | -             # subtract two numbers
       | *             # multiply two numbers
       | /             # divide two numbers
       | =             # equality on numbers
       | <             # less-than on numbers
       | and           # boolean conjunction (NEW)
       | or            # boolean disjunction (NEW)

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
|#


#| (5 pts) Part 1: Scheme0 Parser
   
   In part 1, you will extend the Scheme0 core parser from PA4 to
   support the additional syntactic forms of the Scheme0 source
   language. The updated abstract syntax is given below (the same as
   in PA4 but with a few new constructors). If you don't have a
   working parser from PA4, you can contact the TA (Jacob Schaupp
   js400421@ohio.edu) to obtain a copy to start from.
|#

#| Scheme0 Abstract Syntax |#

data Type:
  | tnum
  | tbool
end

data Unop:
  | unot
  | uneg # new
end

data Binop:
  | add
  | sub
  | mul
  | div
  | equ
  | lt
  | conj # new
  | disj # new
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

# A couple helper functions for binops.
fun is-bool-binop(b :: Binop) -> Boolean:
  cases (Binop) b:
    | equ => true
    | lt => true
    | else => false
  end
end
is-arith-binop :: (Binop -> Boolean) = compose(not, is-bool-binop)

# Predicate asserting that an expression doesn't contain any derived forms.
fun is-core(e :: Exp) -> Boolean:
  cases (Exp) e:
    | val(_) => true
    | ident(_) => true
    | unexp(u, e1) =>
      cases (Unop) u:
        | uneg => false
        | else => is-core(e1)
      end
    | binexp(b, e1, e2) =>
      cases (Binop) b:
        | conj => false
        | disj => false
        | else => is-core(e1) and is-core(e2)
      end
    | ite(e1, e2, e3) => is-core(e1) and is-core(e2) and is-core(e3)
    | letx(x, e1, e2) => is-core(e1) and is-core(e2)
  end
end

# The core language is the subset of the source language that satisfies is-core.
type ExpC = Exp%(is-core)

#| END Scheme0 Abstract Syntax |#

# Example expressions
ex0 = ident("x")
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
ex13 = binexp(conj, val(bool(false)), val(bool(true)))
ex14 = binexp(disj, val(bool(false)), val(bool(true)))
ex15 = binexp(conj, ex13, ex14)
ex20 = unexp(unot, val(bool(true)))
ex21 = unexp(uneg, val(num(5)))
ex22 = binexp(add, unexp(uneg, val(num(2))), unexp(uneg, val(num(-2))))
ex23 = binexp(add, binexp(sub, val(num(0)), val(num(2))), binexp(sub, val(num(0)), val(num(-2))))
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

# Convert an s-expression to a Scheme0 expression.
fun parseExp(s :: Sexp) -> Result<Exp>:
  ... # Fill in here
end

# The overall parser is the composition of parseExp with S.read-s-exp.
fun parse(s :: String) -> Result<Exp>:
  parseExp(S.read-s-exp(s))
where:
  parse("x") is ok(ex0)
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
  parse("(and false true)") is ok(ex13)
  parse("(or false true)") is ok(ex14)
  parse("(and (and false true) (or false true))") is ok(ex15)

  # unexps
  parse("(not true)") is ok(ex20)
  parse("(neg 5)") is ok(ex21)
  parse("(+ (neg 2) (neg -2))") is ok(ex22)
  parse("(+ (- 0 2) (- 0 -2))") is ok(ex23)
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

#| (5 pts) Part 2: Scheme0 Desugarer

   The new syntactic forms are technically not necessary since they
   can be implemented in terms of existing constructs of Scheme0 Core
   (in fact, some of the Scheme0 Core features aren't even necessary
   -- can you guess which ones they are?). That is, they can be
   implemented as "derived forms", or "syntactic sugar", and desugared
   to more primitive language features.

   In this part, you will implement a desugaring pass to eliminate all
   syntactic sugar, resulting in an expression containing only core
   syntax. It should be implemented as a recursive traversal through
   the expression, replacing derived forms with their translations to
   core syntax.
|#

fun desugar(e :: Exp) -> ExpC:
  ... # Fill in here
end

check "desugar(...)":
  desugar(ex13) is ite(val(bool(false)), val(bool(true)), val(bool(false)))
  desugar(ex14) is ite(val(bool(false)), val(bool(true)), val(bool(true)))
  desugar(ex15) is ite(ite(val(bool(false)), val(bool(true)), val(bool(false))),
    ite(val(bool(false)), val(bool(true)), val(bool(true))), val(bool(false)))
  desugar(ex21) is binexp(sub, val(num(0)), val(num(5)))
  desugar(ex22) is ex23
end


#| (10 pts) Part 3: Scheme0 Core Typechecker

   In this part, you will implement a typechecker for Scheme0 Core
   according to the typing relation given in
   doc/scheme0_core_typing.pdf in the course repo.

   That is, define a function 'tycheck' that takes a typing context
   (mapping identifiers to types) and an expression, and returns the
   type of that expression. As in the parser and interpreter, you may
   find it helpful to break the problem down into smaller functions,
   e.g., a function for typechecking unary expressions, a function for
   typechecking binary expressions, etc.
|#

# An environment of A is a function from identifiers (strings) to
# elements of type A.
data Env<A>: env(e :: (String -> Result<A>)) end

# Look up the value bound to an identifier in an environment.
fun lookup<A>(e :: Env<A>, x :: String) -> Result<A>:
  e.e(x)
end

# Update an environment with a new binding.
fun upd<A>(e :: Env<A>, x :: String, new-a :: A) -> Env<A>:
  env(lam(y :: String): if string-equal(x, y): ok(new-a) else: e.e(y) end end)
end

# The initial type environment (typing context) contains no bindings.
init-ctx :: Env<Type> = env(lam(x :: String): err(TypeError(x + " is unbound")) end)

fun valType(v :: Val) -> Type:
  cases (Val) v:
    | bool(_) => tbool
    | num(_) => tnum
  end
end

fun assertType(gamma :: Env<Type>, e :: ExpC, expected :: Type) -> Result<Type>:
  seq(tycheck(gamma, e), lam(ty):
      if eq(ty, expected):
        ok(ty)
      else:
        err(TypeError("assertType: expected " + to-repr(expected)
              + ", got " + to-repr(ty)))
      end
    end)
end

fun tycheckUnexp(gamma :: Env<Type>, u :: Unop, e :: ExpC) -> Result<Type>:
  cases (Unop) u:
    | unot => assertType(gamma, e, tbool)
    | uneg => err(TypeError("uneg should have been desugared"))
  end
end

# Compute the type of expression 'e' under typing context 'gamma'.
fun tycheck(gamma :: Env<Type>, e :: ExpC) -> Result<Type>:
  ... # Fill in here
end

# These tests provide evidence that your typechecker is working properly.
check "tycheck(...)":
  tycheck(init-ctx, ex0) satisfies is-type-error
  tycheck(init-ctx, ex1) is ok(tnum)
  tycheck(init-ctx, ex2) is ok(tnum)
  tycheck(init-ctx, ex3) is ok(tbool)
  tycheck(init-ctx, ex4) is ok(tbool)
  tycheck(init-ctx, ex5) is ok(tnum)
  tycheck(init-ctx, ex6) is ok(tnum)
  tycheck(init-ctx, ex7) is ok(tnum)
  tycheck(init-ctx, ex8) is ok(tnum)
  tycheck(init-ctx, ex9) is ok(tnum)
  tycheck(init-ctx, ex10) is ok(tnum)
  tycheck(init-ctx, ex11) satisfies is-type-error
  tycheck(init-ctx, ex12) is ok(tnum)
  tycheck(init-ctx, desugar(ex13)) is ok(tbool)
  tycheck(init-ctx, desugar(ex14)) is ok(tbool)
  tycheck(init-ctx, desugar(ex15)) is ok(tbool) 
  tycheck(init-ctx, ex20) is ok(tbool)
  tycheck(init-ctx, desugar(ex21)) is ok(tnum)
  tycheck(init-ctx, ex30) is ok(tnum)
  tycheck(init-ctx, ex31) satisfies is-type-error
  tycheck(init-ctx, ex32) satisfies is-type-error
  tycheck(init-ctx, ex33) satisfies is-type-error
  tycheck(init-ctx, ex34) satisfies is-type-error
  tycheck(init-ctx, ex35) is ok(tnum)
  tycheck(init-ctx, ex40) is ok(tnum)
  tycheck(init-ctx, ex41) is ok(tbool)
end


#| Scheme0 Core interpreter

   Since we haven't modified the core language, we don't have to
   extend the interpreter! This is the benefit of syntactic sugar --
   it allows us to extend the source language without complicating the
   internals of the language implementation (e.g., the typechecker and
   interpreter).

   As with the parser, if you don't have a working interpreter from
   PA4 then you can contact the TA (Jacob Schaupp
   js400421@ohio.edu) for a copy. If you already have a working
   interpreter from PA4, then there's nothing for you to do in this
   part besides copying over your implementation and verifying that
   the entire Scheme0 pipeline works as intended.
|#

# Evaluate an expression under a given environment, producing a value.
fun interp(rho :: Env<Val>, e :: ExpC) -> Result<Val>:
  ... # Fill in here
end

# The initial variable environment contains no bindings.
init-env :: Env<Val> = env(lam(x :: String): err(InterpError(x + " is unbound")) end)

# End-to-end pipeline (parser -> desugarer -> typechecker -> interpreter).
fun run(s :: String) -> Result<Val>:
  seq(parse(s), lam(e):
      d = desugar(e)
      seq(tycheck(init-ctx, d), lam(_):
          interp(init-env, d)
        end)
    end)
end

# These tests provide evidence that your interpreter is working properly.
check "interp(...)":
  interp(init-env, ex0) satisfies is-interp-error
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
  interp(init-env, desugar(ex13)) is ok(bool(false))
  interp(init-env, desugar(ex14)) is ok(bool(true))
  interp(init-env, desugar(ex15)) is ok(bool(false))
  interp(init-env, ex20) is ok(bool(false))
  interp(init-env, desugar(ex21)) is ok(num(-5))
  interp(init-env, ex30) is ok(num(3))
  interp(init-env, ex31) satisfies is-interp-error
  interp(init-env, ex32) satisfies is-interp-error
  interp(init-env, ex33) is ok(num(5))
  interp(init-env, ex34) satisfies is-interp-error
  interp(init-env, ex35) is ok(num(6))
  interp(init-env, ex40) is ok(num(-4))
  interp(init-env, ex41) is ok(bool(true))
end

# These tests provide evidence that your parser, desugarer,
# typechecker, and interpreter are all working properly.
check "run(...)":
  run("x") satisfies is-type-error # unbound variable
  run("3") is ok(num(3))
  run("true") is ok(bool(true))
  run("(* 3 4)") is ok(num(12))
  run("(* 3 true)") satisfies is-type-error # boolean argument to multiplication
  run("(let x 3 4)") is ok(num(4))
  run("(let x 3 (* x true))") satisfies is-type-error # boolean argument to multiplication
  run("(4 * 5)") satisfies is-parse-error
  run("(* true false)") satisfies is-type-error # boolean argument to multiplication
  run("(let x 5 (* x x))") is ok(num(25))
  run("(LET x 5 (* x x))") satisfies is-parse-error
  run("(let x 4 (let x 5 (* x x)))") is ok(num(25))
  run("(+ (let x 0 (+ x x)) 0)") is ok(num(0))
  run("(/ (let x 0 0) (let x 0 x))") satisfies is-interp-error # division by zero
  run("(let x (let x (+ 3 4) x) (* x x))") is ok(num(49))
  run("(* x 4)") satisfies is-type-error # unbound variable
  run("(if true 0 true)") satisfies is-type-error # branches have different types
  run("(if false 0 true)") satisfies is-type-error # branches have different types
  run("(/ 1 2)") is ok(num(1/2))
  run("(- 0 (/ 1 2))") is ok(num(-1/2))
  run("(not 123)") satisfies is-type-error # number argument to negation
  run("(let x 2 (let y 3 (let z 5 (- x (* y z)))))") is ok(num(-13))
  run("(if 0 1 2)") satisfies is-type-error # non-boolean discriminee
  run("(+ 123 false)") satisfies is-type-error # boolean argument to addition
  run("(neg 2)") is ok(num(-2))
  run("(and false true)") is ok(bool(false))
  run("(let b true (and b true))") is ok(bool(true))
  run("(let b true (or false b))") is ok(bool(true))
  run("(and (and false true) (or true false))") is ok(bool(false))
  run("(or (and false true) (or true false))") is ok(bool(true))
  run("(or (and false true) (or x false))") satisfies is-type-error # unbound variable
  run("(let x false (or (and false true) (or x false)))") is ok(bool(false))
  run("(let b true (or (and false true) (or b false)))") is ok(bool(true))
  # Add your own test cases here.
end
