data Exp:
  | ENum(n :: Number)
  | EPlus(l :: Exp, r :: Exp) # l + r
  | EDiv(l :: Exp, r :: Exp) # l / r
end

# (5 + 3) / 3    EDiv(EPlus(ENum(5), ENum(3)), 3)

# Sequential composition
fun seq<A, B>(x :: Option<A>, k :: (A -> Option<B>))
  -> Option<B>:
  cases (Option) x:
    | none => none
    | some(a) => k(a)
  end
end

# Parallel composition
fun plus<A>(x :: Option<A>, y :: Option<A>) -> Option<A>:
  cases (Option) x:
    | none => y
    | else => x
  end
end

fun choice<A>(l :: List<Option<A>>) -> Option<A>:
  l.foldr(plus, none)
end

fun lift1<A, B>(f :: (A -> B)) -> (Option<A> -> Option<B>):
  lam(x): seq(x, lam(a): some(f(a)) end) end
end

fun lift2<A, B, C>(f :: (A, B -> C))
  -> (Option<A>, Option<B> -> Option<C>):
  lam(x, y):
    seq(x, lam(a): seq(y, lam(b): some(f(a, b)) end) end)
  end
end

fun num-plus(n :: Number, m :: Number) -> Number:
  n + m
end

fun eval(e :: Exp) -> Option<Number>:
  cases (Exp) e:
    | ENum(n) => some(n)
    | EPlus(l, r) => lift2(num-plus)(eval(l), eval(r))
    | EDiv(l, r) =>
      seq(eval(l), lam(n):
          seq(eval(r), lam(m):
              if m == 0:
                none
              else:
                some(n / m)
              end
            end)
        end)
  end
where:
  eval(ENum(5)) is some(5)
  eval(EPlus(ENum(5), ENum(6))) is some(11)
  eval(EPlus(ENum(10), EPlus(ENum(5), ENum(6)))) is some(21)
  eval(EDiv(ENum(5), ENum(6))) is some(5 / 6)
  eval(EDiv(ENum(5), EPlus(ENum(6), ENum(4)))) is some(1 / 2)
  eval(EDiv(ENum(1), ENum(0))) is none
  eval(EDiv(EPlus(ENum(5), ENum(6)), EPlus(ENum(5), ENum(-5)))) is none
  eval(EPlus(ENum(1), EDiv(ENum(100), EPlus(ENum(5), ENum(-5))))) is none
end

# fun eval(e :: Exp) -> Option<Number>:
#   cases (Exp) e:
#     | ENum(n) => some(n)
#     | EPlus(l, r) =>
#       cases (Option) eval(l):
#         | none => none
#         | some(n) =>
#           cases (Option) eval(r):
#             | none => none
#             | some(m) =>
#               some(n + m)
#           end
#       end
#     | EDiv(l, r) =>
#       cases (Option) eval(l):
#         | none => none
#         | some(n) =>
#           cases (Option) eval(r):
#             | none => none
#             | some(m) =>
#               if m == 0:
#                 none
#               else:
#                 some(n / m)
#               end
#           end
#       end
#   end
# where:
#   eval(ENum(5)) is some(5)
#   eval(EPlus(ENum(5), ENum(6))) is some(11)
#   eval(EPlus(ENum(10), EPlus(ENum(5), ENum(6)))) is some(21)
#   eval(EDiv(ENum(5), ENum(6))) is some(5 / 6)
#   eval(EDiv(ENum(5), EPlus(ENum(6), ENum(4)))) is some(1 / 2)
#   eval(EDiv(ENum(1), ENum(0))) is none
#   eval(EDiv(EPlus(ENum(5), ENum(6)), EPlus(ENum(5), ENum(-5)))) is none
#   eval(EPlus(ENum(1), EDiv(ENum(100), EPlus(ENum(5), ENum(-5))))) is none
# end


data Result<R, E>:
  | err(e :: E)
  | ok(r :: R)
end