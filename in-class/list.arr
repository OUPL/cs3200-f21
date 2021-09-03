
# Compute the length of a list.
fun list-length<A>(l :: List<A>) -> Number:
  ...
where:
  list-length(empty) is 0
  list-length([list:]) is 0
  list-length([list: 5]) is 1
  list-length([list: 5, 7]) is 2
  list-length([list: false, false, true]) is 3
end

# Compute the sum of a list of Numbers.
fun list-sum(l :: List<Number>) -> Number:
  ...
where:
  list-sum(empty) is 0
  list-sum([list: 5]) is 5
  list-sum([list: 5, 7]) is 12
  list-sum([list: 5, 7, 3]) is 15
  list-sum([list: 5, 7, 3, -1]) is 14
end

# Check if an element is in a list.
fun list-member<A>(x :: A, l :: List<A>) -> Boolean:
  ...
where:
  list-member("hello", [list: "hello", "world"]) is true
  list-member("world", [list: "hello", "world"]) is true
  list-member("!", [list: "hello", "world"]) is false
end

# Compute the lengths of a list of Strings.
fun list-str-lens(l :: List<String>) -> List<Number>:
  ...
where:
  list-str-lens([list: "hello", "world", "!"]) is [list: 5, 5, 1]
end

# Remove all non-positive numbers from a list.
fun list-pos-nums(l :: List<Number>) -> List<Number>:
  ...
where:
  list-pos-nums([list: 1, 5, -3, 3, -10, 0, 100]) is [list: 1, 5, 3, 100]
end

# Remove every other element from a list (keeping the first).
fun list-alternating<A>(l :: List<A>) -> List<A>:
  ...
where:
  list-alternating(range(0, 10)) is [list: 0, 2, 4, 6, 8]
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
  ...
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
  ...
where:
  list-avg([list: 0, 100]) is 50
  list-avg([list: 1, 100]) is 50.5
  list-avg([list: 1, 2, 3]) is 2
  list-avg(range(1, 10)) is 5
end

# General map combinator.
fun list-map<A, B>(g :: (A -> B), l :: List<A>) -> List<B>:
  ...
end

# Reimplement list-str-lens using list-map.
fun list-str-lens2(l :: List<String>) -> List<Number>:
  ...
where:
  list-str-lens2([list: "hello", "world", "!"]) is [list: 5, 5, 1]
end

# General filter combinator.
fun list-filter<A>(g :: (A -> Boolean), l :: List<A>) -> List<A>:
  ...
end

# Reimplement list-pos-nums using list-filter.
fun list-pos-nums2(l :: List<Number>) -> List<Number>:
  ...
where:
  list-pos-nums2([list: 1, 5, -3, 3, -10, 0, 100]) is [list: 1, 5, 3, 100]
end

# General fold-right combinator.
fun list-foldr<A, B>(g :: (A, B -> B), l :: List<A>, acc :: B) -> B:
  ...
end

# General fold-left combinator.
fun list-foldl<A, B>(g :: (B, A -> B), acc :: B,  l :: List<A>) -> B:
  ...
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