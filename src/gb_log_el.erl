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

-record(state, {progress=true, log_mode}).

%% API
add() ->
    gen_event:add_handler(error_logger, ?MODULE, []).

remove() ->
    (catch gen_event:delete_handler(error_logger, ?MODULE, delete)).

%% gen_event api
init([]) ->
    {ok, #state{}}.

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

handle_event({info_report, _G, {_, Type, Report}}, State) ->
    log_info_report(Type, Report),
    {ok, State};

handle_event({error_report, _G, {_E, Type, Report}}, State) ->
    log_error_report(Type, Report),
    {ok, State};

handle_event({error, _G, {_E, Type, Report}}, State) ->
    log_error(Type, Report),
    {ok, State};

handle_event({info_msg, _G, {_E, Type, Report}}, State) ->
    log_info(Type, Report),
    {ok, State};

handle_event({Event, _G, {_E, Type, Report}}, State) ->
    log_unknown_report(Event, Type, Report),
    {ok, State}.

handle_call(_Call, State) ->
    ?debug("unhandled call",[]),
    {ok, {error, not_handled}, State}.

handle_info(_Info, State) ->
    ?debug("Unknown Info ~p", [_Info]),    
    {ok, State}.

terminate(_Reason, _State) ->
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
log_info_report(info_report, Report) ->
    ?debug("info: ~p", [Report]),
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
    R = re:replace(R0, "\n", "", [{return, list}, global]),
    ?debug("info: ~s", [R]),
    ok;
log_info(Fmt, Args) when is_list(Fmt), is_list(Args) ->
    ?debug("info: ~s", [io_lib:format(Fmt, Args)]),
    ok;
log_info(Type, Report) ->
    ?debug("info: ~p ~p", [Type, Report]),
    ok.

%% log_error
log_error(_, [R0]) ->
    R = re:replace(R0, "\n", " ", [{return, list}, global]),
    ?error("~s", [R]),
    ok;
log_error(T, R) ->
    ?error("~p ~p", [T, R]),
    ok.

%% log_unknown_report
log_unknown_report(Event, Type, Report) ->
    ?debug("event: ~p ~p ~p", [Event, Type, Report]),
    ok.
