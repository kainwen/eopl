-module(binary_tree).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([leaf/1, interior_node/3, move_up/1, at_root/1, at_leaf/1]).

-export([init/1, handle_call/3]).

% APIs

leaf(N) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [N], []),
    Pid.

interior_node(N, Left, Right) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [N], []),
    ok = set_element(Left, set_parent, Pid),
    ok = set_element(Right, set_parent, Pid),
    ok = set_element(Pid, set_left, Left),
    ok = set_element(Pid, set_right, Right),
    Pid.

move_up(TreeNode) ->
    get_element(TreeNode, get_parent).

at_root(TreeNode) ->
    get_element(TreeNode, get_parent) =:= {}.

at_leaf(TreeNode) ->
    L = get_element(TreeNode, get_left),
    R = get_element(TreeNode, get_right),
    L =:= {} andalso R =:= R.

% Callbacks

init([N]) ->
    {ok, {N, {}, {}, {}}}.

handle_call({set_left, Left}, _From, {Num, LeftSon, RightSon, Parent}) ->
    {reply, ok, {Num, Left, RightSon, Parent}};
handle_call({set_right, Right}, _From, {Num, LeftSon, RightSon, Parent}) ->
    {reply, ok, {Num, LeftSon, Right, Parent}};
handle_call({set_parent, P}, _From, {Num, LeftSon, RightSon, Parent}) ->
    {reply, ok, {Num, LeftSon, RightSon, P}};
handle_call(get_left, _From, St={Num, LeftSon, RightSon, Parent}) ->
    {reply, LeftSon, St};
handle_call(get_right, _From, St={Num, LeftSon, RightSon, Parent}) ->
    {reply, RightSon, St};
handle_call(get_parent, _From, St={Num, LeftSon, RightSon, Parent}) ->
    {reply, Parent, St};
handle_call(get_info, _From, St={Num, LeftSon, RightSon, Parent}) ->
    {reply, Num, St}.

% private
set_element(TreeNode, Msg, Element) ->
    ok = gen_server:call(TreeNode, {Msg, Element}).

get_element(TreeNode, Msg) ->
    gen_server:call(TreeNode, Msg).

% unit test
binary_tree_test() ->
    Tree = interior_node(1,
                         interior_node(2, leaf(3), leaf(4)),
                         interior_node(5, leaf(6), leaf(7))),
    ?assert(at_root(Tree) =:= true),
    ?assert(at_leaf(Tree) =:= false),
    Left = get_element(Tree, get_left),
    ?assert(at_root(Left) =:= false),
    ?assert(at_leaf(Left) =:= false),
    Back = move_up(Left),
    ?assert(at_root(Back) =:= true),
    ?assert(get_element(Back, get_info) =:= 1),
    LL = get_element(Left, get_left),
    ?assert(at_leaf(LL) =:= true).
