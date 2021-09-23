

data Stream<A>:
  | lz-link(first :: A, rest :: ( -> Stream<A>))
end

fun nats-from(n :: Number) -> Stream<Number>:
  lz-link(n, lam(): nats-from(n + 1) end)
end

nats :: Stream<Number> = nats-from(0)

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