%%%===================================================================
%% @author Jonas Falkevik
%% @copyright 2016 Pundun Labs AB
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
%% simple trace module
%% stop tracing after time and/or the number of trace msgs received.

-module(gb_trace).
-export([pid/2, pid/3,
	 calls/1, calls/2,
	 pattern/3, rem_pattern/3,
	 stop/0]).

%% Internal API exports
-export([trace_/4]).

-include("gb_log.hrl").
-define(OUT(Fmt, Args), ?debug(Fmt, Args)).

-define(def_opts, [{msgs, 100}, {time, 10000}]).
-define(osts, os:timestamp()).

-record(state, {pid, flags, time, msgs, tmfas}).
-record(mfa, {mod, func}).

%% API
pattern(String, MS, Flags) when is_list(String) ->
    MFAs = scan(String),
    send_req(pattern, {MFAs, MS, Flags, true}).

rem_pattern(String, MS, Flags) when is_list(String) ->
    MFAs = scan(String),
    send_req(pattern, {MFAs, MS, Flags, false}).

stop() ->
    send_req(stop,[]).

calls(String) ->
    calls(String, ?def_opts).

calls(String, Opts) ->
    MFAs = scan(String),
    spawn_link(?MODULE, trace_, [all, [call], MFAs, Opts]).

pid(Pid, Flags) when is_pid(Pid), is_list(Flags) ->
    pid(Pid, Flags, ?def_opts).

pid(Pid, Flags, Opts) when is_pid(Pid),
			     is_list(Flags),
			     is_list(Opts) ->
    spawn_link(?MODULE, trace_, [Pid, Flags, [], Opts]).

%% Internal functions
send_req(Req, ReqArg) ->
    send_req(Req, ReqArg, 5000).
send_req(Req, ReqArgs, To) ->
    Ref = erlang:make_ref(),
    erlang:send(?MODULE, {Req, {Ref, self()}, ReqArgs}),
    receive 
	{Ref, Res} ->
	    Res
    after To ->
	{error, timeout}
    end.

reply_to({Ref, From}, R) ->
    From ! {Ref, R}.

generating_mfas([MFA = {M,_F,_A}|R]) when M =/= '_' ->
    [MFA | generating_mfas(R)];
generating_mfas([{'_',F,A} | R]) ->
    Modules = erlang:loaded(),
    [{M,F,A} || M <- Modules] ++ generating_mfas(R);
generating_mfas([]) ->
    [].

trace_(Pid, Flags, MFAs, Opts) ->
    true = erlang:register(?MODULE, self()),
    erlang:trace(Pid, true, Flags),
    TMFAs = generating_mfas(MFAs),
    [erlang:trace_pattern(TMFA, true, [local]) || TMFA <- TMFAs],
    Time = proplists:get_value(time, Opts, 10000),
    Msgs = proplists:get_value(msgs, Opts, 1000),
    trace_loop(#state{pid = Pid, flags = Flags, time = Time, msgs = Msgs, tmfas=TMFAs}).

trace_loop(S = #state{msgs = M, time = Time, tmfas = TMFAs0}) when M > 0  ->
    Ts = ?osts,
    receive 
	{pattern, From, {MFAs, _MS, Flags, Enable}} ->
	    TMFAs = generating_mfas(MFAs),	    
	    R = [erlang:trace_pattern(TMFA, Enable, Flags) || TMFA <- TMFAs],
	    reply_to(From, R),
	    Ts2 = ?osts,
	    trace_loop(S#state{time = rem_time(Time, Ts2, Ts), tmfas=TMFAs0 ++ TMFAs});
	{stop, From, []} ->
	    reply_to(From, ok),
	    stop(S, explicit_stop);
	{trace, Pid, Type, Trace} ->
	    Ts2 = ?osts,
	    ?OUT("trace ~10000p ~10000p ~10000p ~10000p", [calendar:now_to_datetime(Ts2), Pid, Type, Trace]),
	    trace_loop(S#state{msgs = rem_msg(M), time = rem_time(Time, Ts2, Ts) });
	{trace_ts, Pid, Type, Trace, TraceTs} ->
	    Ts2 = ?osts,
	    ?OUT("trace ~10000p ~10000p ~10000p ~10000p", [calendar:now_to_datetime(TraceTs), Pid, Type, Trace]),
	    trace_loop(S#state{msgs = rem_msg(M) , time = rem_time(Time, Ts2, Ts) });
	Trace ->
	    Ts2 = ?osts,
	    ?OUT("trace ~10000p ~10000p", [calendar:now_to_datetime(Ts2), Trace]),
	    trace_loop(S#state{msgs = rem_msg(M), time = rem_time(Time, Ts2, Ts) })
    after Time ->
	    stop(S, {stop, time})
    end;
trace_loop(S) ->
    stop(S, {stop, msgs}).
		
stop(#state{pid = Pid, flags = Flags, tmfas = TMFAs}, Reason) ->
    erlang:trace(Pid, false, Flags),
    [erlang:trace_pattern(TMFA, false, [local]) || TMFA <- TMFAs],
    ?OUT("~p done ~p", [?MODULE, Reason]).

%% infinite - elapsed = infinte
rem_time(infinity, _, _) ->
    infinity;
%% real time conversion
rem_time(Time, EndTs, StartTs) ->
    rem_time(Time, timer:now_diff(EndTs, StartTs) / 1000).
rem_time(Time, Elapsed) ->
    case Time - Elapsed of
	NewTime when NewTime >= 0 ->
	    trunc(NewTime);
	 _ ->
	    0
    end.

rem_msg(infinity) ->
    check_msg_queue(infinity, 0);

rem_msg(M) when is_integer(M) ->
    M - 1.

check_msg_queue(Ok, Stop) ->
    {message_queue_len, L} = erlang:process_info(self(), message_queue_len),
    case L of
	L when L > 10000 ->
	    Stop;
	_ ->
	    Ok
    end.

%% Scan string for erlang modules and functions to be traced
scan(String) when is_list(String) ->
    {ok, Scan, 1} = erl_scan:string(String),
    iterate(mod, Scan, #mfa{}, []).

%% Iterate over scanned data
iterate(mod, [{Type,1,Mod}, {':',1} | R], MFA, Aux) when Type =:= var; 
							 Type =:= atom ->
    iterate(func, R, MFA#mfa{mod=Mod}, Aux);
iterate(mod, [_ | R], MFA, Aux)  ->
    iterate(func, R, MFA, Aux);

iterate(func, [{_, 1, Func}, {'/',_}, {integer, 1, Arity}| R], MFA, Aux) ->
    iterate(mod, R, #mfa{}, [{MFA#mfa.mod, Func, Arity} | Aux]);
iterate(func, [{_, 1, Func}| R], MFA, Aux) ->
    iterate(mod, R, #mfa{}, [{MFA#mfa.mod, Func, '_'} | Aux]);

iterate(_, [],_,Aux) ->
    Aux.
