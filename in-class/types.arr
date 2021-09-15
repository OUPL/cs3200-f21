data Zero:
end

fun exfalso<A>(z :: Zero) -> A:
  cases (Zero) z:
  end
end

data One:
  | one
end

data Two:
  | O
  | I
end

fun two-to-bool(t :: Two) -> Boolean:
  cases (Two) t:
    | O => false
    | I => true
  end
end

fun bool-to-two(b :: Boolean) -> Two:
  if b:
    I
  else:
    O
  end
end


# ∀ t :: Two, bool-to-two(two-to-bool(t)) = t
# ∀ b :: Boolean, two-to-bool(bool-to-two(b)) = b

check:
  bool-to-two(two-to-bool(O)) is O
  bool-to-two(two-to-bool(I)) is I

  two-to-bool(bool-to-two(false)) is false
  two-to-bool(bool-to-two(true)) is true
end


data Pair<A, B>:
  | pair(a :: A, b :: B)
end

fun swap<A, B>(p :: Pair<A, B>) -> Pair<B, A>:
  cases (Pair) p:
    | pair(a, b) => pair(b, a)
  end
end

data Either<A, B>:
  | left(a :: A)
  | right(b :: B)
end

fun coswap<A, B>(e :: Either<A, B>) -> Either<B, A>:
  cases (Either) e:
    | left(a) => right(a)
    | right(b) => left(b)
  end
end