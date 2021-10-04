
data Box<A>: box(ref v :: A) end

b1 = box(1)

# print(b1!v)

b1!{v : 5}

# print(b1!v)

fun mutate(x :: Box<Number>, y :: Box<Number>) -> {}:
  block:
    x!{v : 1}
    x!{v : y!v}
    {}
  end
end

fun mutate2(x :: Box<Number>, y :: Box<Number>) -> {}:
  block:
    x!{v : y!v}
    {}
  end
end


# fun list-length(l :: List<Number>) -> Number:
#   cases (List) l:
#     | empty => 0
#     | link(_, r) => 1 + list-length(r)
#   end
# end

var dummy :: (List<Number> -> Number) = lam(_): 0 end

list-length :: (List<Number> -> Number) =
  lam(l):
    cases (List) l:
      | empty => 0
      | link(_, r) => 1 + dummy(r)
    end
  end

dummy := list-length

fun mk-counter() -> ( -> Number):
  ctr = box(0)
  lam():
    block:
      ctr!{v : (ctr!v + 1)}
      ctr!v
    end
  end
end

counter :: ( -> Number) = mk-counter()

print(counter())
print(counter())



fun replace<A>(n :: Number, v :: A, l :: List<A>) -> List<A>:
  cases (List) l:
    | empty => empty
    | link(f, r) =>
      if n <= 0:
        link(v, r)
      else:
        link(f, replace(n - 1, v, r))
      end
  end
where:
  replace(2, 5, [list: 1, 2, 3, 6]) is [list:1, 2, 5, 6]
end