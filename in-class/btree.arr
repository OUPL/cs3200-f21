data BTree<A>:
  | leaf(x :: A)
  | node(l :: BTree<A>, r :: BTree<A>)
end

fun tree-elems<A>(t :: BTree<A>) -> List<A>:
  cases (BTree) t:
    | leaf(x) => link(x, empty)
    | node(l, r) => tree-elems(l).append(tree-elems(r))
  end
where:
  tree-elems(node(leaf(2), leaf(3))) is [list: 2, 3]
end