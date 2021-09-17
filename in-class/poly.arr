include pick

data StringNum:
  | stringnum(s :: String, n :: Number)
end

data NumString:
  | numstring(n :: Number, s :: String)
end

fun stringnum-to-numstring(sn :: StringNum) -> NumString:
  cases (StringNum) sn:
    | stringnum(s, n) => numstring(n, s)
  end
end

fun numstring-to-stringnum(ns :: NumString) -> StringNum:
  cases (NumString) ns:
    | numstring(n, s) => stringnum(s, n)
  end
end

data BoolNum:
  | boolnum(b :: Boolean, n :: Number)
end


data Pair<A, B>:
  | pair(a :: A, b :: B)
end

# Pair<Number, String>
# pair(a :: Number, b :: String)

# Pair<String, Boolean>

fun swap<A, B>(p :: Pair<A, B>) -> Pair<B, A>:
  cases (Pair) p:
    | pair(x, y) => pair(y, x)
  end
end

data PolyList<A>:
  | poly-empty
  | poly-link(x :: A, r :: PolyList<A>)
end

# PolyList<Boolean>

data Zero:
end

# PolyList<Zero>

fun mkZero() -> Zero:
  mkZero()
end

# print(poly-link(mkZero(), poly-empty))

poly-empty
poly-link(false, poly-empty)
poly-link(true, poly-empty)
poly-link(false, poly-link(false, poly-empty))
poly-link(false, poly-link(true, poly-empty))

fun poly-length<A>(l :: PolyList<A>) -> Number:
  cases (PolyList) l:
    | poly-empty => 0
    | poly-link(_, r) => 1 + poly-length(r)
  end
end

print(poly-empty)
print("\n")

print(poly-link(123, poly-empty))
print("\n")



data MyType1:
  | MyType1Con1
  | MyType1Con2
  | MyType1Con3
end

data MyType2:
  | MyType2Con1(b1 :: Boolean, b2 :: Boolean,
      b3 :: Boolean)
  | MyType2Con2(b :: Boolean, mt :: MyType1)
end

# 2*2*2 + 2*3 = 8 + 6 = 14

data MyType3:
  | MyType3Con1(b :: Boolean, n :: Number)
  | MyType3Con2(b :: Boolean, mt :: MyType1)
end


fun an-elt<A>(s :: Set<A>) -> A:
  cases (Pick) s.pick():
    | pick-none => raise("empty set")
    | pick-some(e, r) => e
  end
end

# data Unit:
#   | tt
# end

# fun print-elts<A>(s :: Set<A>) -> Unit:
#   cases (Pick) s.pick():
#     | pick-none => tt
# end