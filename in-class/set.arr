type LSet = List

mt-set = empty

fun is-in<A>(x :: A, s :: LSet<A>) -> Boolean:
  cases (LSet) s:
    | empty => false
    | link(f, r) => (f == x) or is-in(x, r)
  end
end

fun insert<A>(x :: A, s :: LSet<A>) -> LSet<A>:
  if s.member(x):
    s
  else:
    link(x, s)
  end
end

fun union<A>(s1 :: LSet<A>, s2 :: LSet<A>) -> LSet<A>:
  cases (LSet) s1:
    | empty => s2
    | link(f, r) => insert(f, union(r, s2))
  end
end

fun size<A>(s :: LSet<A>) -> Number:
  s.length()
end

fun to-list<A>(s :: LSet<A>) -> List<A>:
  s
end