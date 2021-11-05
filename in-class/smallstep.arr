

data Exp:
  | ENum(n :: Number)
  | EPlus(l :: Exp, r :: Exp) # l + r
  | EDiv(l :: Exp, r :: Exp) # l / r
end

fun step(e :: Exp) -> Exp:
  cases (Exp) e:
    | ENum(n) => ENum(n)
    | EPlus(e1, e2) =>
      cases (Exp) e1:
        | ENum(n) =>
          cases (Exp) e2:
            | ENum(m) =>
              ENum(n + m)
            | else => EPlus(e1, step(e2))
          end
        | else => EPlus(step(e1), e2)
      end
    | EDiv(e1, e2) =>
      cases (Exp) e1:
        | ENum(n) =>
          cases (Exp) e2:
            | ENum(m) =>
              if m == 0:
                e
              else:
                ENum(n / m)
              end
            | else => EDiv(e1, step(e2))
          end
        | else => EDiv(step(e1), e2)
      end
  end
end

fun eval(e :: Exp) -> Exp:
  e2 = step(e)
  if e2 == e:
    e
  else:
    eval(e2)
  end
end