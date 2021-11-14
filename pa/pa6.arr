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

fun seq3<A, B, C, D>(x :: Result<A>, y :: Result<B>, z :: Result<C>,
    k :: (A, B, C -> Result<D>)) -> Result<D>:
  seq(x, lam(a): seq(y, lam(b): seq(z, lam(c): k(a, b, c) end) end) end)
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


#| In this assignment, you'll be extending Scheme1 Core to Typed
   Scheme1 Core. In part 1, you'll extend the parser to support the
   extended syntax given below (which includes types and 'rec'
   expressions). In part 2, you'll update the desugarer to account for
   the syntax extensions from part 1. In part 3, you'll implement a
   typechecker!

   Note that our old trick for encoding recursion no longer works once
   we add a type system (try to recreate the 'omega' example from PA5
   in the new typed language -- it won't be typeable!), so to maintain
   support for recursive computations, we must add a new
   syntactic form to the core language for recursive bindings. An
   expression of the form '(rec x t e)' should be understood as "let x
   stand for recursive occurrences of e in e, where e (and thus x as
   well) has type t".

   Thus, in part 4, you must extend the interpreter to support
   recursive expressions. Good luck!

   Types
   t ::= bool
       | num
       | (-> t1 t2)
   
   Unary Operators
   u ::= not           # negate a boolean
       | neg           # negate a number (derived)

   Binary Operators
   b ::= +             # add two numbers
       | -             # subtract two numbers
       | *             # multiply two numbers
       | /             # divide two numbers
       | =             # equality on numbers
       | <             # less-than on numbers
       | and           # boolean conjunction (derived)
       | or            # boolean disjunction (derived)

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
       | (let x t e2 e3) # let expression (derived)
       | (fun x t e)     # anonymous function with parameter x of type t and body e
       | (e1 e2)         # function application (e1 applied to argument e2)
       | (rec x t e)     # NEW: recursive definition. let x stand for recursive occurrences in e.
|#


#| (5 pts) Part 1: Typed Scheme1 Parser

   Extend the Scheme1 parser to parse types (now included in type
   annotations on 'let', 'fun', and 'rec' expressions) as well as
   'rec' expressions.
|#

#| Scheme1 Abstract Syntax |#

data Type:
  | tbool
  | tnum
  | tarrow(t1 :: Type, t2 :: Type)
end

data Unop:
  | unot
  | uneg
end

data Binop:
  | add
  | sub
  | mul
  | div
  | equ
  | lt
  | conj
  | disj
end

data Val:
  | bool(b :: Boolean)
  | num(n :: Number)
    # closures are created by the interpreter and don't appear in source programs.
  | clos(env :: Env<Exp>, x :: String, body :: Exp)
end

data Exp:
  | val(v :: Val)
  | ident(s :: String)
  | unexp(op :: Unop, e :: Exp)
  | binexp(op :: Binop, e1 :: Exp, e2 :: Exp)
  | ite(e1 :: Exp, e2 :: Exp, e3 :: Exp)
  | letx(x :: String, t :: Type, e1 :: Exp, e2 :: Exp)
  | fn(x :: String, t :: Type, body :: Exp)
  | app(e1 :: Exp, e2 :: Exp)
  | recx(x :: String, t :: Type, e :: Exp)
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
    | letx(_, _, _, _) => false
    | fn(_, _, e1) => is-core(e1)
    | app(e1, e2) => is-core(e1) and is-core(e2)
    | recx(_, _, e1) => is-core(e1)
  end
end

# The core language is the subset of the source language that satisfies is-core.
type ExpC = Exp%(is-core)

#| END Scheme1 Abstract Syntax |#

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
ex30 = letx("x", tnum, val(num(3)), ident("x"))
ex31 = letx("yzw", tnum, binexp(add, val(num(4)), ident("x")), ident("x"))
ex32 = letx("x", tnum, binexp(add, val(num(4)), binexp(add, ident("x"), ident("x"))),
  binexp(mul, ident("x"), ident("y")))
ex33 = ite(val(bool(true)), val(num(5)), ident("x"))
ex34 = ite(val(bool(false)), val(num(5)), ident("x"))
ex35 = letx("x", tnum, val(num(6)), ite(val(bool(false)), val(num(5)), ident("x")))
ex40 = letx("b", tbool,
  ite(binexp(lt, val(num(1)), val(num(1))), val(num(4)), val(num(5))),
  binexp(sub, ident("b"), val(num(9))))
ex41 = letx("q", tbool, binexp(equ, val(num(3)), val(num(3))), ident("q"))
ex50 = fn("x", tnum, ident("x"))
ex51 = fn("y", tnum, binexp(add, ident("y"), val(num(1))))
ex52 = fn("x", tbool, unexp(unot, ident("x")))
ex53 = fn("x", tnum, fn("y", tnum, binexp(add, ident("x"), ident("y"))))
ex60 = app(ex50, val(num(123)))
ex61 = app(ex51, val(num(2)))
ex62 = app(ex51, val(bool(false)))
ex63 = app(ex52, val(bool(false)))
ex64 = app(ex52, val(num(3)))
ex65 = app(ex60, val(bool(true)))
ex66 = app(ex53, val(num(3)))
ex67 = app(ex66, val(num(5)))
ex68 = app(app(ex53, val(num(1))), val(bool(true)))
# Ω = (λx. x x) (λx. x x) NO LONGER TYPE-ABLE, but should still parse
Omega = app(fn("x", tarrow(tnum, tnum), app(ident("x"), ident("x"))),
  fn("x", tarrow(tnum, tnum), app(ident("x"), ident("x"))))

# Divergence is still possible.
ex70 = recx("x", tnum, ident("x"))

# Multiplication from repeated addition is still possible using 'rec'.
nat-mult = recx("mult", tarrow(tnum, tarrow(tnum, tnum)),
  fn("m", tnum, fn("n", tnum, ite(binexp(equ, ident("n"), val(num(0))), val(num(0)),
        binexp(add, ident("m"), app(app(ident("mult"), ident("m")),
            binexp(sub, ident("n"), val(num(1)))))))))

# Type synonym for the s-expression type.
type Sexp = S.S-Exp

# Convert an s-expression to a Typed Scheme1 expression.
fun parseExp(s :: Sexp) -> Result<Exp>:
  ... # Fill in here (start by copying over your parsing code from PA5)
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
  parse("(let x num 3 x)") is ok(ex30)
  parse("( let  yzw num (+ 4 x) x  )") is ok(ex31)
  parse("(let x num (+ 4 (+ x x)) (* x y))") is ok(ex32)
  # parse("  ( let x (+ 4 (+ x x)) (*x  y  ) )") satisfies is-parse-error
  parse("(let x num 3 x x)") satisfies is-parse-error
  parse("(let b bool (if (< 1 1) 4 5) (- b 9))") is ok(ex40)
  parse("(let q bool (= 3 3) q)") is ok(ex41)

  # if expressions
  parse("(if true 5 x)") is ok(ex33)
  parse("(if false 5 x)") is ok(ex34)
  parse("(let x num 6 (if false 5 x))") is ok(ex35)
  parse("(cons false 5 x)") satisfies is-parse-error
  parse("(if false 5 x y)") satisfies is-parse-error

  # functions
  parse("(fun x num x)") is ok(ex50)
  parse("(fun y num (+ y 1))") is ok(ex51)
  parse("(fun x bool (not x))") is ok(ex52)
  parse("(fun x num (fun y num (+ x y)))") is ok(ex53)

  # function application
  parse("((fun x num x) 123)") is ok(ex60)
  parse("((fun y num (+ y 1)) 2)") is ok(ex61)
  parse("((fun y num (+ y 1)) false)") is ok(ex62)
  parse("((fun x bool (not x)) false)") is ok(ex63)
  parse("((fun x bool (not x)) 3)") is ok(ex64)
  parse("(((fun x num x) 123) true)") is ok(ex65)
  parse("((fun x num (fun y num (+ x y))) 3)") is ok(ex66)
  parse("(((fun x num (fun y num (+ x y))) 3) 5)") is ok(ex67)
  parse("(((fun x num (fun y num (+ x y))) 1) true)") is ok(ex68)
  parse("((fun x (-> num num) (x x)) (fun x (-> num num) (x x)))") is ok(Omega)

  # recursive definitions
  parse("(rec x num x)") is ok(ex70)
  parse("(rec mult (-> num (-> num num)) (fun m num (fun n num (if (= n 0) 0 (+ m ((mult m) (- n 1)))))))") is ok(nat-mult)
end


#| (2 pts) Part 2: Typed Scheme1 Desugarer

   Now, extend the desugarer to account for the new syntax. There
   isn't much to do here since the syntax extensions are fairly
   trivial.
|#

fun desugar(e :: Exp) -> ExpC:
  ... # Fill in here (start by copying over your desugarer code from PA5)
end

check "desugar(...)":
  desugar(ex13) is ite(val(bool(false)), val(bool(true)), val(bool(false)))
  desugar(ex14) is ite(val(bool(false)), val(bool(true)), val(bool(true)))
  desugar(ex15) is ite(ite(val(bool(false)), val(bool(true)), val(bool(false))),
    ite(val(bool(false)), val(bool(true)), val(bool(true))), val(bool(false)))
  desugar(ex21) is binexp(sub, val(num(0)), val(num(5)))
  desugar(ex22) is ex23
  desugar(ex30) is app(ex50, val(num(3)))
  desugar(ex31) is app(fn("yzw", tnum, ident("x")), binexp(add, val(num(4)), ident("x")))
  desugar(ite(val(bool(true)),
      letx("x", tnum, val(num(1)), ident("x")), letx("x", tnum, val(num(2)), ident("x")))) is
  ite(val(bool(true)), app(ex50, val(num(1))), app(ex50, val(num(2))))
  desugar(recx("x", tbool, binexp(conj, val(bool(true)), ident("x")))) is
  recx("x", tbool, ite(val(bool(true)), ident("x"), val(bool(false))))
end


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


#| (10 pts) Part 3: Scheme1 Core Typechecker

   Now, it's time to implement a typechecker for Scheme1 Core! Write
   a function 'tycheck' that computes the type of a Scheme1 Core
   expression under a given typing context (or producing an error if
   the expression is not typeable).

   Refer to the typing relation document at
   https://github.com/OUPL/cs3200-f21/blob/master/doc/scheme1_core_typing.pdf for
   the typing rules. Recall that the Greek letter Γ (capital "gamma")
   stands for the typing context, and the notation "Γ, x:T" stands for
   Γ updated with a new binding that maps variable x to type T.

|#

# The initial type environment (the typing context) contains no bindings.
init-ctx :: Env<Type> = env(lam(x :: String): err(TypeError(x + " is unbound")) end)

# Typecheck a Scheme1 Core expression under typing context gamma.
fun tycheck(gamma :: Env<Type>, e :: Exp) -> Result<Type>:
  ... # Fill in here
end

check "tycheck(..)":
  tycheck(init-ctx, desugar(ex0)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex1)) is ok(tnum)
  tycheck(init-ctx, desugar(ex2)) is ok(tnum)
  tycheck(init-ctx, desugar(ex3)) is ok(tbool)
  tycheck(init-ctx, desugar(ex4)) is ok(tbool)
  tycheck(init-ctx, desugar(ex5)) is ok(tnum)
  tycheck(init-ctx, desugar(ex6)) is ok(tnum)
  tycheck(init-ctx, desugar(ex7)) is ok(tnum)
  tycheck(init-ctx, desugar(ex8)) is ok(tnum)
  tycheck(init-ctx, desugar(ex9)) is ok(tnum)
  tycheck(init-ctx, desugar(ex10)) is ok(tnum)
  tycheck(init-ctx, desugar(ex11)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex12)) is ok(tnum)
  tycheck(init-ctx, desugar(ex13)) is ok(tbool)
  tycheck(init-ctx, desugar(ex14)) is ok(tbool)
  tycheck(init-ctx, desugar(ex15)) is ok(tbool)
  tycheck(init-ctx, desugar(ex20)) is ok(tbool)
  tycheck(init-ctx, desugar(ex21)) is ok(tnum)
  tycheck(init-ctx, desugar(ex22)) is ok(tnum)
  tycheck(init-ctx, desugar(ex23)) is ok(tnum)
  tycheck(init-ctx, desugar(ex30)) is ok(tnum)
  tycheck(init-ctx, desugar(ex31)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex32)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex33)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex34)) satisfies is-type-error # unbound variable
  tycheck(init-ctx, desugar(ex35)) is ok(tnum)
  tycheck(init-ctx, desugar(ex40)) satisfies is-type-error # bool - num
  tycheck(init-ctx, desugar(ex41)) is ok(tbool)
  tycheck(init-ctx, desugar(ex50)) is ok(tarrow(tnum, tnum))
  tycheck(init-ctx, desugar(ex51)) is ok(tarrow(tnum, tnum))
  tycheck(init-ctx, desugar(ex52)) is ok(tarrow(tbool, tbool))
  tycheck(init-ctx, desugar(ex53)) is ok(tarrow(tnum, tarrow(tnum, tnum)))
  tycheck(init-ctx, fn("b", tbool, ite(ident("b"), val(num(1)), val(num(0)))))
    is ok(tarrow(tbool, tnum))
  tycheck(init-ctx, desugar(ex60)) is ok(tnum)
  tycheck(init-ctx, desugar(ex61)) is ok(tnum)
  tycheck(init-ctx, desugar(ex62)) satisfies is-type-error # expects num argument
  tycheck(init-ctx, desugar(ex63)) is ok(tbool)
  tycheck(init-ctx, desugar(ex64)) satisfies is-type-error # expects bool argument
  tycheck(init-ctx, desugar(ex65)) satisfies is-type-error # can't apply number
  tycheck(init-ctx, desugar(ex66)) is ok(tarrow(tnum, tnum))
  tycheck(init-ctx, desugar(ex67)) is ok(tnum)
  tycheck(init-ctx, desugar(ex68)) satisfies is-type-error # expects num second argument
  tycheck(init-ctx, desugar(Omega)) satisfies is-type-error
  tycheck(init-ctx, desugar(ex70)) is ok(tnum)
  tycheck(init-ctx, desugar(nat-mult)) is ok(tarrow(tnum, tarrow(tnum, tnum)))
end

#| Part 4 (3 pts) Scheme1 Core interpreter

   Lastly, you must extend the interpreter to support recursive
   definitions. You can consult the bigstep semantics document at
   https://github.com/OUPL/cs3200-f21/blob/master/doc/scheme1_core_bigstep.pdf
   to see how it should be done.
|#

# Evaluate an expression under a given environment, producing a value.
fun interp(rho :: Env<Exp>, e :: ExpC) -> Result<Val>:
  ... # Fill in here (start by copying over your interpreter code from PA5)
end

# The initial variable environment contains no bindings.
init-env :: Env<Exp> = env(lam(x :: String): err(InterpError(x + " is unbound")) end)

# End-to-end pipeline (parser -> desugarer -> typechecker -> interpreter).
fun run(s :: String) -> Result<Val>:
  seq(parse(s), lam(e):
      d = desugar(e)
      seq(tycheck(init-ctx, d), lam(_):
          interp(init-env, desugar(e))
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
  interp(init-env, desugar(ex30)) is ok(num(3))
  interp(init-env, desugar(ex31)) satisfies is-interp-error
  interp(init-env, desugar(ex32)) satisfies is-interp-error
  interp(init-env, ex33) is ok(num(5))
  interp(init-env, ex34) satisfies is-interp-error
  interp(init-env, desugar(ex35)) is ok(num(6))
  interp(init-env, desugar(ex40)) is ok(num(-4))
  interp(init-env, desugar(ex41)) is ok(bool(true))  
  interp(init-env, desugar(ex60)) is ok(num(123))
  interp(init-env, desugar(ex61)) is ok(num(3))
  interp(init-env, desugar(ex62)) satisfies is-interp-error
  interp(init-env, desugar(ex63)) is ok(bool(true))
  interp(init-env, desugar(ex64)) satisfies is-interp-error
  interp(init-env, desugar(ex65)) satisfies is-interp-error
  interp(init-env, app(app(nat-mult, val(num(5))), val(num(6)))) is ok(num(30))
end

# These tests provide evidence that your parser, desugarer,
# typechecker, and interpreter are all working properly.
check "run(...)":
  run("x") satisfies is-type-error # unbound variable
  run("3") is ok(num(3))
  run("true") is ok(bool(true))
  run("(* 3 4)") is ok(num(12))
  run("(* 3 true)") satisfies is-type-error # boolean argument to multiplication
  run("(let x num 3 4)") is ok(num(4))
  run("(let x num 3 (* x true))") satisfies is-type-error # boolean argument to multiplication
  run("(4 * 5)") satisfies is-parse-error
  run("(* true false)") satisfies is-type-error # boolean argument to multiplication
  run("(let x num 5 (* x x))") is ok(num(25))
  run("(LET x num 5 (* x x))") satisfies is-parse-error
  run("(let x num 4 (let x num 5 (* x x)))") is ok(num(25))
  run("(+ (let x num 0 (+ x x)) 0)") is ok(num(0))
  run("(/ (let x num 0 0) (let x num 0 x))") satisfies is-interp-error # division by zero
  run("(let x num (let x num (+ 3 4) x) (* x x))") is ok(num(49))
  run("(* x 4)") satisfies is-type-error # unbound variable
  run("(if true 0 true)") satisfies is-type-error # branch type mismatch
  run("(if false 0 true)") satisfies is-type-error # branch type mismatch
  run("(/ 1 2)") is ok(num(1/2))
  run("(- 0 (/ 1 2))") is ok(num(-1/2))
  run("(not 123)") satisfies is-type-error # number argument to negation
  run("(let x num 2 (let y num 3 (let z num 5 (- x (* y z)))))") is ok(num(-13))
  run("(if 0 1 2)") satisfies is-type-error # non-boolean discriminee
  run("(+ 123 false)") satisfies is-type-error # boolean argument to addition
  run("(neg 2)") is ok(num(-2))
  run("(and false true)") is ok(bool(false))
  run("(let b bool true (and b true))") is ok(bool(true))
  run("(let b bool true (or false b))") is ok(bool(true))
  run("(and (and false true) (or true false))") is ok(bool(false))
  run("(or (and false true) (or true false))") is ok(bool(true))
  run("(or (and false true) (or x false))") satisfies is-type-error # unbound variable
  run("(let x bool false (or (and false true) (or x false)))") is ok(bool(false))
  run("(let b bool true (or (and false true) (or b false)))") is ok(bool(true))
  run("((fun x num x) 123)") is ok(num(123))
  run("((fun y num (+ y 1)) 2)") is ok(num(3))
  run("((fun y bool (+ y 1)) false)") satisfies is-type-error # bool + num
  run("((fun x bool (not x)) false)") is ok(bool(true))
  run("((fun x bool (not x)) 3)") satisfies is-type-error # boolean negation on number
  run("(((fun x num x) 123) true)") satisfies is-type-error # applying non-function
  run("(((fun m num (fun n num (/ m n))) 10) 5)") is ok(num(2))
  run("(((fun m num (fun n num (/ m n))) 10) (- 1 1))") satisfies is-interp-error # division by zero
  run("(let f (-> num num) (rec f (-> num num) (fun n num (if (< n 1) 0 (+ n (f (- n 1)))))) (f 10))") is ok(num(55))
  # Add your own test cases here.
end