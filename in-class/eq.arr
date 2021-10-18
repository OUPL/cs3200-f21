fun eq<A>(x :: A, y :: A) -> Boolean:
  x == y
end

# fun eq<A>(x :: A) -> (A -> Boolean):
#   lam(y):
#     x == y
#   end
# end


fun list-any<A>(f :: (A -> Boolean), l :: List<A>)
  -> Boolean:
  cases (List) l:
    | empty => false
    | link(x, r) => f(x) or list-any(f, r)
  end
end


 # x :: String
 # l :: List<String>

 # list-any(eq(x), l)


fun curry<A, B, C>(f :: (A, B -> C)) -> (A -> (B -> C)):
  lam(a): lam(b): f(a, b) end end
end

# Curried eq.
eqc = curry(eq)
