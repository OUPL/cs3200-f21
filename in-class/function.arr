
fun twice(f :: (Number -> Number), n :: Number) -> Number:
  f(f(n))
end

# fun add1(n :: Number) -> Number:
#   n + 1
# end

# print(twice(add1, 2))

print(twice(lam(n): n + 1 end, 2))