-module(reg).

-include("common.hrl").

-compile(export_all).

set_reg(exp, V) ->
    ets:update_element(?ETS_TAB, ?ID, {#machine.reg_exp, V});
set_reg(env, V) ->
    ets:update_element(?ETS_TAB, ?ID, {#machine.reg_env, V});
set_reg(cont, V) ->
    ets:update_element(?ETS_TAB, ?ID, {#machine.reg_cont, V});
set_reg(val, V) ->
    ets:update_element(?ETS_TAB, ?ID, {#machine.reg_val, V});
set_reg(proc1, V) ->
    ets:update_element(?ETS_TAB, ?ID, {#machine.reg_proc1, V}).

get_reg(exp) ->
    ets:lookup_element(?ETS_TAB, ?ID, #machine.reg_exp);
get_reg(env) ->
    ets:lookup_element(?ETS_TAB, ?ID, #machine.reg_env);
get_reg(cont) ->
    ets:lookup_element(?ETS_TAB, ?ID, #machine.reg_cont);
get_reg(val) ->
    ets:lookup_element(?ETS_TAB, ?ID, #machine.reg_val);
get_reg(proc1) ->
    ets:lookup_element(?ETS_TAB, ?ID, #machine.reg_proc1).
