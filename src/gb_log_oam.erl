%%%===================================================================
%% @author Jonas Falkevik
%% @copyright 2015 Pundun Labs AB
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------
%% @title
%% @doc
%% Module Description:
%% @end
%%%===================================================================

-module(gb_log_oam).
-include("gb_log.hrl").
-export([read/0,
	 read/1,
	 available_filters/0,
	 load_filter/1,
	 load_store_filters_beam/0,
	 load_default_filter/0,
	 make_filter/1,
	 make_filters_mod/1]).

read() ->
    read(filename:join(code:priv_dir(gb_log),"filters.cfg")).

read(File) ->
    {ok, Data} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Data)),
    {ok, Filters} = gb_log_cfg:parse(Tokens),
    [make_filter(Filter) || Filter <- Filters].    


available_filters() ->
    [F || {F,_} <- erlang:get_module_info(gb_log_filters, exports), F =/= module_info].

load_filter(Name) ->
    code:load_binary(gb_log_filter, [], gb_log_filters:Name()).

load_store_filters_beam() ->
    Filters = read(),
    Beam = make_filters_mod(Filters),
    File = filename:join(code:priv_dir(gb_log), "gb_log_filters.beam"),
    file:write_file(File, Beam).

load_priv_beam() ->
    FileName = filename:join(code:priv_dir(gb_log),"gb_log_filters.beam"),
    {ok, Beam} = file:read_file(FileName),
    code:load_binary(gb_log_filters, "", Beam).

load_default_filter() ->
    catch load_priv_beam(),
    case catch code:load_binary(gb_log_filter, [], gb_log_filters:default()) of
	{module, gb_log_filter} ->
	    ok;
	_E ->
	    error_logger:error_info("gb_log: no default filter found, missing configuration; falling back to debug"),
	    make_filters_mod([make_filter([{name, auto_fallback}, {level, debug}])]),
	    code:load_binary(gb_log_filter, [], gb_log_filters:auto_fallback()),
	    {error, missing_default_config}
    end.


getval(Tag, TVs) ->
    case lists:keyfind(Tag, 1, TVs) of
	{Tag, Value} ->
	    Value;
	_ ->
	    {error, undefined}
    end.

make_filter({_NamedRec,TVs}) ->
    make_filter(TVs);

make_filter(TVs) ->
    Level = getval(level, TVs),
    Name  = getval(name, TVs),
    CEForms= make_mod(Name, Level),
    {ok, _, Beam } = compile:forms(CEForms, [from_core, binary]),
    {Name, Beam}.


make_filters_mod(Filters) ->
    Name = cerl:c_atom(gb_log_filters),
    FilterNames = [cerl:c_fname(F, 0) || {F,_} <- Filters],
   
    CEForms = 
	cerl:c_module(Name,
		      FilterNames ++ [cerl:c_fname(module_info,0),
				      cerl:c_fname(module_info,1)],
		      [make_filter_load(Filter) || Filter <- Filters] ++
				mod_info(Name)
		      ),
    {ok, _, Beam} = compile:forms(CEForms, [from_core, binary]),
    code:load_binary(gb_log_filters, "", Beam),
    Beam.

make_filter_load({Name, Beam}) ->
    %% cerl:c_binary/1 does not compile, but building binary as a c_literal works
    Bin = {c_literal, [], Beam},
    {cerl:c_fname(Name, 0), cerl:c_fun([], Bin)}.

make_mod(FilterName, Level0) ->
    Level = levelname_to_int(Level0),
    Name = cerl:c_atom(gb_log_filter),
    cerl:c_module(
		Name,
		[cerl:c_fname(filter, 1),
		 cerl:c_fname(level, 0),
		 cerl:c_fname(name, 0),
		 cerl:c_fname(record,0),
		 cerl:c_fname(module_info, 0), 
		 cerl:c_fname(module_info, 1)],
		 [make_filter_fun(Level),
		  make_simple_fun(level, Level0),
		  make_simple_fun(name, FilterName),
		  make_simple_fun(record, #lf{})] ++ 
		  mod_info(Name)).

make_simple_fun(Name, Ret) when is_atom(Name), is_atom(Ret) ->
    CELevel = cerl:c_atom(Ret),
    {cerl:c_fname(Name, 0), cerl:c_fun([], CELevel)};
make_simple_fun(Name, Ret) when is_atom(Name), is_tuple(Ret) ->
    CELevel = {c_literal, [], Ret},
    {cerl:c_fname(Name, 0), cerl:c_fun([], CELevel)}.

make_filter_fun(CfgLevel0) ->
    init_gen_var(),
    Arg1 = cerl:c_var('FuncArg1'),

    Clauses = make_match_clauses(Arg1, CfgLevel0),
    
    Var = gen_var(),
    LastClause = cerl:c_clause([Var], cerl:c_atom(true), cerl:c_atom(ok)),
    Case    = cerl:c_case(Arg1, Clauses ++ [LastClause]),
    {cerl:c_fname(filter, 1),cerl:c_fun([Arg1],Case)}. 


make_match_clauses(Arg, CfgLevel0) ->
    make_match_clauses(Arg, CfgLevel0, []).
make_match_clauses(Arg, CfgLevel0, Aux) -> 
    CfgLevel = cerl:c_int(CfgLevel0),
    VarLevel = cerl:c_var('Level'),
    Tuple = make_match_field(record, #lf{level = VarLevel}), 
    EqC = make_call(erlang, '>=', [VarLevel, CfgLevel]),
    GblogC = make_call(gb_log, log, [Arg]),
    Clause = cerl:c_clause([Tuple], EqC, GblogC),
    lists:reverse([Clause|Aux]).

make_match_field(record, Tuple) when is_tuple(Tuple) ->
    make_match_field(record, tuple_to_list(Tuple));
make_match_field(record, [Name | Fields]) ->
    cerl:c_tuple(make_match(Fields, [cerl:c_atom(Name)])).

%make_match(List) ->
%    make_match(List, []).
make_match([undefined|R], Aux) ->
    make_match(R, [gen_var()|Aux]);
make_match([Field|R], Aux) ->
    make_match(R, [Field|Aux]);
make_match([], Aux) ->
    lists:reverse(Aux).
    
%make_match_field(record, Tuple, Matches) when is_tuple(Tuple) ->
%    make_match_field(record, tuple_to_list(Tuple), Matches);
%make_match_field(record, [Name | Fields], Matches) ->
%    cerl:c_tuple(make_base_match(Fields, [cerl:c_atom(Name)], Matches)).

%make_base_match([Field|R], Aux, Matches) ->
%    case lists:keyfind(Field, 1, Matches) of
%	{Field, Replace} ->
%	    make_base_match(R, [Replace| Aux], Matches);
%	_ ->
%	    make_base_match(R, [gen_var()|Aux], Matches)
%    end;
%make_base_match([], Aux, _Matches) ->
%    lists:reverse(Aux).

%% non-tail recursive but fast 
%setnth(1, [_|Rest], New) -> [New|Rest];
%setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

init_gen_var() ->
    put(generated_var,0).


%% Generate variable names to not overlap
%% But
gen_var() ->
    EVar = get(generated_var),
    put(generated_var, EVar + 1),
    cerl:c_var(list_to_atom("_var" ++ integer_to_list(EVar))).

make_call(Mod0, Fun0, Args) ->
    Mod = cerl:c_atom(Mod0),
    Fun = cerl:c_atom(Fun0),
    cerl:c_call(Mod, Fun, Args).
    
%make_fun(Args, Body) ->
%    cerl:c_fun(Args, Body).


mod_info(Name) ->
	M = cerl:c_atom(erlang),
	F = cerl:c_atom(get_module_info),
	Info0 = {cerl:c_fname(module_info, 0),
		cerl:c_fun([], cerl:c_call(M, F, [Name]))},
	
	Key = cerl:c_var('Key'),
	Info1 = {cerl:c_fname(module_info, 1),
		cerl:c_fun([Key], cerl:c_call(M, F, [Name, Key]))},
	[Info0, Info1].

levelname_to_int(debug) ->
    ?debug_level;
levelname_to_int(info) ->
    ?info_level;
levelname_to_int(notice) ->
    ?notice_level;
levelname_to_int(warning) ->
    ?warning_level;
levelname_to_int(error) ->
    ?error_level;
levelname_to_int(critical) ->
    ?critical_level;
levelname_to_int(alert) ->
    ?alert_level;
levelname_to_int(emergency) ->
    ?emergency_level.
