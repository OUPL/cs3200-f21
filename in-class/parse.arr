import s-exp as S

fun eq<A>(x :: A, y :: A) -> Boolean:
  x == y
end

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

# Lift a function of two arguments to the Result type.
fun lift2<A, B, C>(f :: (A, B -> C)) -> (Result<A>, Result<B> -> Result<C>):
  lam(x, y): seq(x, lam(a): seq(y, lam(b): ok(f(a, b)) end) end) end
end

# Function application in the Result type (chickenbutt).
fun ap<A, B>(f :: Result<(A -> B)>, x :: Result<A>) -> Result<B>:
  seq(f, lam(g): seq(x, lam(a): ok(g(a)) end) end)
end


type Sexp = S.S-Exp

data ArithC:
  | numC(n :: Number)
  | plusC(l :: ArithC, r :: ArithC)
  | multC(l :: ArithC, r :: ArithC)
end

fun parseNum(s :: Sexp) -> Result<ArithC>:
  cases (Sexp) s:
    | s-num(n) => ok(numC(n))
    | else => err("parseNum")
  end
end

fun asList(s :: Sexp) -> Result<List<Sexp>>:
  cases (Sexp) s:
    | s-list(l) => ok(l)
    | else => err("asList")
  end
end

fun asList3(s :: Sexp) -> Result<{Sexp; Sexp; Sexp}>:
  seq(asList(s), lam(l):
      if l.length() == 3:
        ok({l.get(0); l.get(1); l.get(2)})
      else:
        err("asList3")
      end
    end)
end

fun asSym(s :: Sexp, expected :: String) -> Result<{}>:
  cases (Sexp) s:
    | s-sym(sym) =>
      if eq(sym, expected):
        ok({})
      else:
        err("asSym")
      end
    | else => err("asSym")
  end
end

fun parsePlus(s :: Sexp) -> Result<ArithC>:
  seq(asList3(s), lam({s1 :: Sexp; s2; s3}):
      seq(asSym(s1, "+"), lam(_):
          seq(parse(s2), lam(a1):
              seq(parse(s3), lam(a2):
                  ok(plusC(a1, a2))
                end)
            end)
        end)
    end)
end

fun parseMult(s :: Sexp) -> Result<ArithC>:
  cases (Sexp) s:
    | s-list(l) =>
      if l.length() == 3:
        cases (Sexp) l.get(0):
          | s-sym(sym) =>
            if eq(sym, "*"):
              seq(parse(l.get(1)), lam(a1):
                  seq(parse(l.get(2)), lam(a2):
                      ok(multC(a1, a2))
                    end)
                end)
            else:
              err("parsePlus")
            end
          | else =>
            err("parsePlus")
        end
      else:
        err("parsePlus")
      end
    | else => err("parsePlus")
  end
end

fun parse(s :: Sexp) -> Result<ArithC>:
  plus(parseNum(s), plus(parsePlus(s), parseMult(s)))
end

check:
  fun p(s): parse(S.read-s-exp(s)) end
  p("3") is ok(numC(3))
  p("(+ 1 2)") is ok(plusC(numC(1), numC(2)))
  p("(* (+ 1 2) (* 2 5))") is
    ok(multC(plusC(numC(1), numC(2)), multC(numC(2), numC(5))))
end