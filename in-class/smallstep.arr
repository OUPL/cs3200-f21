
data Exp:
  | ENum(n :: Number)
  | ETrue
  | EFalse
  | EPlus(e1 :: Exp, e2 :: Exp)
  | EEq(e1 :: Exp, e2 :: Exp)
  | EIte(e1 :: Exp, e2 :: Exp, e3 :: Exp)
end

fun is-value(e :: Exp) -> Boolean:
  is-ENum(e) or is-ETrue(e) or is-EFalse(e)
end

fun step(e :: Exp) -> Exp:
  cases (Exp) e:
    | ENum(_) => e
    | ETrue => e
    | EFalse => e
    | EPlus(e1, e2) =>
      cases (Exp) e1:
        | ENum(n1) =>
          cases (Exp) e2:
            | ENum(n2) => ENum(n1 + n2)
            | else => EPlus(e1, step(e2))
          end
        | else => EPlus(step(e1), e2)
      end
    | EEq(e1, e2) =>
      cases (Exp) e1:
        | ENum(n1) =>
          cases (Exp) e2:
            | ENum(n2) => if n1 == n2: ETrue else: EFalse end
            | else => EEq(e1, step(e2))
          end
        | else => EEq(step(e1), e2)
      end
    | EIte(e1, e2, e3) =>
      cases (Exp) e1:
        | ETrue => e2
        | EFalse => e3
        | else => EIte(step(e1), e2, e3)
      end
  end
end

fun star_step(e :: Exp) -> Exp:
  e2 = step(e)
  if e2 == e:
    e
  else:
    star_step(e2)
  end
end
