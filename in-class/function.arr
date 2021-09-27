

# fun plus(n :: Number, m :: Number) -> Number:
#   n + m
# end

# fun plus(n :: Number) -> (Number -> Number):
#   lam(m): n + m end
# end


# curried
plus :: (Number -> (Number -> Number)) =
  lam(n): lam(m): n + m end end

l :: List<Number> = [list: 1, 2, 3]

check:
  # map(lam(m): plus(1, m) end, l) is [list: 2, 3, 4]

  map(plus(1), l) is [list: 2, 3, 4]
end

# plus(1) :: Number -> Number

print(plus(1)(2))


fun curry<A, B, C>(f :: (A, B -> C)) -> (A -> (B -> C)):
  lam(a):
    lam(b):
      f(a, b)
    end
  end
end

fun uncurry<A, B, C>(f :: (A -> (B -> C))) -> (A, B -> C):
  ...
end

data Foo:
  | Bar(n :: Number)
  | Baz(s :: String)
end

# fun foo-eq(x :: Foo, y :: Foo) -> Boolean:
#   cases (Foo) x:
#     | Bar(n) =>
#       cases (Foo) y:
#         | Bar(m) => n == m
#         | else => false
#       end
#     | Baz(s1) =>
#       cases (Foo) y:
#         | Baz(s2) => s1 == s2
#         | else => false
#       end
#   end
# end

# 

fun fun-eq(f :: (Boolean -> Number), g :: (Boolean -> Number)) -> Boolean:
  (f(false) == g(false)) and (f(true) == g(true))
end

print(fun-eq(lam(b :: Boolean): 1 end, lam(b :: Boolean): 0 end))