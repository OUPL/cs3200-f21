include either
# fun id<A>(x :: Any) -> A:
#   id(x)
# end

fun f<A, B>(l :: List<A>) -> B:
  cases (List) l:
    | empty => b
    | link(a, r) => list-length(r) 
  end
  
# Compute the length of a list.
fun list-length<A>(l :: List<A>) -> Number:
  cases (List) l:
    | empty => 0
    | link(_, r) => 1 + list-length(r)
  end
where:
  list-length(empty) is 0
  list-length([list:]) is 0
  list-length([list: 5]) is 1
  list-length([list: 5, 7]) is 2
  list-length([list: false, false, true]) is 3
end

# Compute the sum of a list of Numbers.
fun list-sum(l :: List<Number>) -> Number:
  cases (List) l:
    | empty => 0
    | link(f, r) => f + list-sum(r)
  end
where:
  list-sum(empty) is 0
  list-sum([list: 5]) is 5
  list-sum([list: 5, 7]) is 12
  list-sum([list: 5, 7, 3]) is 15
  list-sum([list: 5, 7, 3, -1]) is 14
end

# Check if an element is in a list.
fun list-member<A>(x :: A, l :: List<A>) -> Boolean:
  cases (List) l:
    | empty => false
    | link(f, r) => (f == x) or list-member(x, r)
  end
where:
  list-member("hello", [list: "hello", "world"]) is true
  list-member("world", [list: "hello", "world"]) is true
  list-member("!", [list: "hello", "world"]) is false
end

# Compute the lengths of a list of Strings.
fun list-str-lens(l :: List<String>) -> List<Number>:
  cases (List) l:
    | empty => empty
    | link(f, r) => link(string-length(f), list-str-lens(r))
  end
where:
  list-str-lens([list: "hello", "world", "!"]) is [list: 5, 5, 1]
end

# Remove all non-positive numbers from a list.
fun list-pos-nums(l :: List<Number>) -> List<Number>:
  cases (List) l:
    | empty => empty
    | link(f, r) => if f > 0:
        link(f, list-pos-nums(r))
      else:
        list-pos-nums(r)
      end
  end
where:
  list-pos-nums([list: 1, 5, -3, 3, -10, 0, 100]) is [list: 1, 5, 3, 100]
end

# Remove every other element from a list (keeping the first).
fun list-alternating<A>(l :: List<A>) -> List<A>:
  cases (List) l:
    | empty => empty
    | link(f, r) =>
      cases (List) r:
        | empty => link(f, empty)
        | link(_, r2) => link(f, list-alternating(r2))
      end
  end
where:
  list-alternating(range(0, 10)) is [list: 0, 2, 4, 6, 8]
end

# Append two lists together.
fun list-append<A>(l1 :: List<A>, l2 :: List<A>) -> List<A>:
  ...
where:
  list-append([list: 1, 2], [list: 5]) is [list: 1, 2, 5]
end

# Compute the nth triangular number.
fun tri(n :: Number) -> Number:
  if n <= 0:
    0
  else:
    n + tri(n - 1)
  end
end

# Compute a list of running sums from a list of numbers.
fun list-running-sum(l :: List<Number>) -> List<Number>:
  fun list-running-sum-aux(ll :: List<Number>, acc :: Number) -> List<Number>:
    cases (List) ll:
      | empty => empty
      | link(f, r) => link(acc + f, list-running-sum-aux(r, acc + f))
    end
  end
  list-running-sum-aux(l, 0)
where:
  list-running-sum([list:]) is [list:]
  list-running-sum([list: 1]) is [list: 1]
  list-running-sum([list: 1, 2]) is [list: 1, 3]
  list-running-sum([list: 1, 2, 3]) is [list: 1, 3, 6]
  list-running-sum([list: 1, 2, 3, 4]) is [list: 1, 3, 6, 10]
  list-running-sum(range(0, 50)) is range(0, 50).map(tri)
end

# Compute the average of a list of numbers.
fun list-avg(l :: List<Number>) -> Number:
  list-sum(l) / list-length(l)
where:
  list-avg([list: 0, 100]) is 50
  list-avg([list: 1, 100]) is 50.5
  list-avg([list: 1, 2, 3]) is 2
  list-avg(range(1, 10)) is 5
end

fun list-concat(l :: List<String>) -> String:
  ...
where:
  list-concat([list: "hello ", "world", "!"]) is "hello world!"
end

fun list-map<A, B>(g :: (A -> B), l :: List<A>) -> List<B>:
  cases (List) l:
    | empty => empty
    | link(x, r) => link(g(x), list-map(g, r))
  end
where:
  list-map(lam(n): n + 1 end, [list: 0, 1, 2]) is [list: 1, 2, 3]
end

# l1 :: List<Either<String, Boolean>> =
#   list-map(left, [list: "hello", " world", "!"])

# Reimplement list-str-lens using list-map.
fun list-str-lens2(l :: List<String>) -> List<Number>:
  list-map(string-length, l)
where:
  list-str-lens2([list: "hello", "world", "!"]) is [list: 5, 5, 1]
end

fun list-filter<A>(g :: (A -> Boolean), l :: List<A>) -> List<A>:
  cases (List) l:
    | empty => empty
    | link(x, r) =>
      if g(x):
        link(x, list-filter(g, r))
      else:
        list-filter(g, r)
      end
  end
end

# Reimplement list-pos-nums using list-filter.
fun list-pos-nums2(l :: List<Number>) -> List<Number>:
  list-filter(lam(n): n > 0 end, l)
where:
  list-pos-nums2([list: 1, 5, -3, 3, -10, 0, 100]) is [list: 1, 5, 3, 100]
end

# General fold-right combinator.
fun list-foldr<A, B>(g :: (A, B -> B), l :: List<A>, b :: B) -> B:
  cases (List) l:
    | empty => b
    | link(x, r) => g(x, list-foldr(g, r, b))
  end
where:
  list-foldr(lam(n, m): n + m end, [list: 1, 2, 3], 0) is 6
end

# General fold-left combinator.
fun list-foldl<A, B>(g :: (B, A -> B), acc :: B,  l :: List<A>) -> B:
  cases (List) l:
    | empty => acc
    | link(x, r) => list-foldl(g, g(acc, x), r)
  end
end

# Reimplement list-length using list-foldl.
fun list-foldl-length<A>(l :: List<A>) -> Number:
  ...
where:
  list-foldl-length(empty) is 0
  list-foldl-length([list:]) is 0
  list-foldl-length([list: 5]) is 1
  list-foldl-length([list: 5, 7]) is 2
  list-foldl-length([list: false, false, true]) is 3
end

# Reimplement list-sum using list-foldl.
fun list-foldl-sum(l :: List<Number>) -> Number:
  ...
where:
  list-foldl-sum(empty) is 0
  list-foldl-sum([list: 5]) is 5
  list-foldl-sum([list: 5, 7]) is 12
  list-foldl-sum([list: 5, 7, 3]) is 15
  list-foldl-sum([list: 5, 7, 3, -1]) is 14
end

# Reimplement list-member using list-foldl.
fun list-foldl-member<A>(x :: A, l :: List<A>) -> Boolean:
  ...
where:
  list-foldl-member("hello", [list: "hello", "world"]) is true
  list-foldl-member("world", [list: "hello", "world"]) is true
  list-foldl-member("!", [list: "hello", "world"]) is false
end

# Reimplement list-map using list-foldr.
fun list-foldr-map<A, B>(g :: (A -> B), l :: List<A>) -> List<B>:
  ...
end

# Reimplement list-str-lens using list-foldr-map.
fun list-str-lens3(l :: List<String>) -> List<Number>:
  ...
where:
  list-str-lens3([list: "hello", "world", "!"]) is [list: 5, 5, 1]
end
