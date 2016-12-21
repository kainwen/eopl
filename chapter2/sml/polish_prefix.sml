datatype polish_prefix_abt = None | Const_exp of int | Diff_exp of polish_prefix_abt * polish_prefix_abt
datatype polish_prefix_term = Term of int | Minus

fun parse_polish_prefix_helper([]) = (None, [])
  | parse_polish_prefix_helper(Minus :: rems) =
    let val (op1, r) = parse_polish_prefix_helper(rems) in
        let val (op2, rr) = parse_polish_prefix_helper(r) in
            (Diff_exp(op1, op2), rr)
        end
    end
  | parse_polish_prefix_helper(Term(n) :: rems) =
    (Const_exp(n), rems)

fun parse_polish_prefix(prefix_exp) =
  let val (result, _) = parse_polish_prefix_helper(prefix_exp) in
      result
  end

fun unparse_polish_prefix(prefix_abt) =
  case prefix_abt of
      Const_exp(n) => [Term(n)]
    | Diff_exp(op1, op2) =>
      let val r = [Minus]
          val r1 = unparse_polish_prefix(op1)
          val r2 = unparse_polish_prefix(op2)
      in
          List.concat([r, r1, r2])
      end;
