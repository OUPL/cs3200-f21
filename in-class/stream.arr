
data Stream<A>:
  | lz-link(first :: A, rest :: ( -> Stream<A>))
end

fun lz-first<A>(s :: Stream<A>) -> A:
  cases (Stream) s:
    | lz-link(x, _) => x
  end
end

fun lz-rest<A>(s :: Stream<A>) -> Stream<A>:
  cases (Stream) s:
    | lz-link(_, r) => r()
  end
end

fun nats-from(n :: Number, step :: Number) -> Stream<Number>:
  lz-link(n, lam(): nats-from(n + step, step) end)
end

nats :: Stream<Number> = nats-from(0, 1)
evens :: Stream<Number> = nats-from(0, 2)
odds :: Stream<Number> = nats-from(1, 2)

fun approx<A>(s :: Stream<A>, n :: Number) -> List<A>:
  if n <= 0:
    empty
  else:
    link(lz-first(s), approx(lz-rest(s), n - 1))
  end
end

check:
  approx(nats, 5) is [list: 0, 1, 2, 3, 4]
  approx(evens, 5) is [list: 0, 2, 4, 6, 8]
  approx(odds, 5) is [list: 1, 3, 5, 7, 9]
end


fun lz-map<A, B>(f :: (A -> B), s :: Stream<A>) -> Stream<B>:
  lz-link(f(lz-first(s)), lam(): lz-map(f, lz-rest(s)) end)
end

fun lz-filter<A>(f :: (A -> Boolean), s :: Stream<A>) ->
  Stream<A>:
  ...
end