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
%%%===================================================================

-module(gb_log_el).

-include("gb_log.hrl").
-behaviour(gen_event).

%% API
-export([add/0,
	 remove/0]).

-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {progress, log_mode}).

%% API
add() ->
    Handlers = gen_event:which_handlers(error_logger),
    case lists:member(?MODULE, Handlers) of
	true ->
	    ok;
	_ ->
	    error_logger:add_report_handler(?MODULE, [])
    end.

remove() ->
    (catch error_logger:delete_report_handler(?MODULE)).

%% gen_event api
init([]) ->
    {ok, #state{progress=true}}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------

handle_event({Type, _G, {_E, progress, Report}}, State) ->
    if State#state.progress ->
	    log_info_report(Type, Report);
	true ->
	    ok
    end,
    {ok, State};

handle_event({info_report, _G, {_Pid, Type, Report}}, State) ->
    log_info_report(Type, Report),
    {ok, State};

handle_event({error_report, _G, {_Pid, Type, Report}}, State) ->
    log_error_report(Type, Report),
    {ok, State};

handle_event({warning_msg, _G, {_Pid, Fmt, Data}}, State) ->
    log_warning(Fmt, Data),
    {ok, State};

handle_event({error, _G, {_Pid, Fmt, Data}}, State) ->
    log_error(Fmt, Data),
    {ok, State};

handle_event({info_msg, _G, {_Pid, Fmt, Data}}, State) ->
    log_info(Fmt, Data),
    {ok, State};

handle_event({Event, _G, {_E, Type, Report}}, State) ->
    log_unknown_report(Event, Type, Report),
    {ok, State};

handle_event(E, State) ->
    ?debug("unhandled event: ~p", [E]),
    {ok, State}.

handle_call(_Call, State) ->
    ?debug("unhandled call",[]),
    {ok, {error, not_handled}, State}.

handle_info(_Info, State) ->
    ?info("Unknown Info ~p", [_Info]),
    {ok, State}.

terminate(Reason, _State) ->
    ?info("terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
%%

%% log_info_report
log_info_report(info_report, [{application, App}, What]) ->
    ?info("application ~p ~p", [App, What]),
    ok;
log_info_report(info_report, [{supervisor, {local, Sup}}, {started, Args}]) ->
    Pid	 = proplists:get_value(pid, Args),
    Name = proplists:get_value(name, Args),
    ?debug("supervisor ~p started ~p(~p)", [Sup, Name, Pid]),
    ok;
log_info_report(info_report, [{supervisor, {_, Sup}}, {started, Args}]) ->
    Pid	 = proplists:get_value(pid, Args),
    Name = proplists:get_value(name, Args),
    ?debug("supervisor ~p started ~p(~p)", [Sup, Name, Pid]),
    ok;
log_info_report(info_report, R0) ->
    R1 = re:replace(R0, "\n", "", [{return, list}, global]),
    R2 = re:replace(R1, "\r", "", [{return, list}, global]),
    R = re:replace(R2, "\t", " ", [{return, list}, global]),

    ?debug("info: ~p", [R]),
    ok;
log_info_report(std_info, [{application, App}|Rest]) ->
    ?debug("application ~p ~p", [App, Rest]),
    ok;
log_info_report(std_info, Report) ->
    ?debug("info: ~p", [Report]),
    ok;
log_info_report(Type, Report) ->
    ?debug("info: ~p ~p", [Type, Report]),
    ok.

%% log_error_report
log_error_report(crash_report, Report) ->
    ?error("CRASH: ~p", [Report]),
    ok;
log_error_report(supervisor_report, Report) ->
    ?warning("SUPERVISOR: ~p", [Report]),
    ok;
log_error_report(Type, Report) ->
    ?warning("ERROR: ~p ~p", [Type, Report]),
    ok.

%% log_info
log_info(R0, []) ->
    R = re:replace(R0, "~n", "", [{return, list}, global]),
    ?info("info: ~s", [R]),
    ok;
log_info(Fmt, Args) when is_list(Fmt), is_list(Args) ->
    ?info("info: ~s", [io_lib:format(Fmt, Args)]),
    ok;
log_info(Type, Report) ->
    ?info("info: ~p ~p", [Type, Report]),
    ok.

%% log_error
log_error(F0, Data) ->
    F = re:replace(F0, "~n", " ", [{return, list}, global]),
    ?error(F, Data),
    ok.

%% log_warning
log_warning(F0, Data) ->
    F = re:replace(F0, "~n", " ", [{return, list}, global]),
    ?warning(F, Data),
    ok.

%% log_unknown_report
log_unknown_report(Event, Type, Report) ->
    ?debug("event: ~p ~p ~p", [Event, Type, Report]),
    ok.
