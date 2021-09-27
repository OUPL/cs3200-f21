
data BT:
  | leaf
  | node(v :: Number, l :: BT, r :: BT)
end

fun is-in-bt(n :: Number, t :: BT) -> Boolean:
  cases (BT) t:
    | leaf => false
    | node(v, l, r) =>
      (v == n) or is-in-bt(n, l) or is-in-bt(n, r)
  end
end

fun all-bt(f :: (Number -> Boolean), t :: BT) -> Boolean:
  cases (BT) t:
    | leaf => true
    | node(v, l, r) =>
      f(v) and all-bt(f, l) and all-bt(f, r)
  end
end

fun is-a-bst(b :: BT) -> Boolean:
  cases (BT) b:
    | leaf => true
    | node(v, l, r) =>
      all-bt(lam(n): n <= v end, l) and
      all-bt(lam(n): n >= v end, r) and
      is-a-bst(l) and is-a-bst(r)
  end
end

# check:
#   node(5, node(3, leaf, node(6, leaf, leaf)), leaf)
#     satisfies is-a-bst # FALSE!
# end