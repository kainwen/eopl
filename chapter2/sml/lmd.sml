datatype lc_exp =
         var_exp of string
         | lambda_exp of string * lc_exp
         | app_exp of lc_exp * lc_exp;

fun occurs_free(v, var_exp(var)) = (v = var)
  | occurs_free(v, lambda_exp(var, exp)) =
    if v = var then
        false
    else
        occurs_free(v, exp)
  | occurs_free(v, app_exp(e1, e2)) = occurs_free(v, e1) orelse occurs_free(v, e2);
