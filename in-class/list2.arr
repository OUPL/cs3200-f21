data Either<A, B>:
  | left(a :: A)
  | right(b :: B)
end

# l1 :: List<Either<String, Number>> =
#   [list: left("hello"), right(124)]

fun f(x :: Number) -> Either<Number, Boolean>:
  if x < 0:
    left(x)
  else:
    right(true)
  end
end

fun g(y :: Number) -> Boolean:
  y >= 0
end

cases (Either) f(-123):
  | left(n) => g(n)
  | right(b) => ...
end

# g(f(-123))

fun zip<A, B>(l1 :: List<A>, l2 :: List<B>) ->
  List<{A; B}>:
  cases (List) l1:
    | empty => empty
    | link(x, xs) =>
      cases (List) l2:
        | empty => empty
        | link(y, ys) =>
          link({x; y}, zip(xs, ys))
      end
  end
end

fun unzip<A, B>(l :: List<{A; B}>) -> {List<A>; List<B>}:
  ...
end

