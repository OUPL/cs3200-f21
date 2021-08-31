
data Nat:
  | O
  | S(n :: Nat)
end

fun nat-plus(n :: Nat, m :: Nat) -> Nat:
  cases (Nat) n:
    | O => m
    | S(nn) => S(nat-plus(nn, m))
  end
end

fun number-to-nat(n :: Number) -> Nat:
  if n <= 0:
    O
  else:
    S(number-to-nat(n - 1))
  end
end

number-to-nat(5)