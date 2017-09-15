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

-module(gb_log).
-record(state, {type, filefd, udpfd, remhost, remport, node,
		fname, fsize, nfiles}).
-include("gb_log.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("gb_conf/include/gb_conf.hrl").

-export([init/0,
	 start_link/0]).

-export([log/1,
	 log_loop/2,
	 tester/1,
	 tester/2]).

tester(S) ->
    ?debug("tjenare: ~s", [S]).

tester(S, Args) ->
    ?debug(S, Args).

format_time({H,M,S}) ->
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H,M,S]));
format_time(_) ->
    {error, invalid_time_format}.
format_date({Y,M,D}) ->
    lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Y,M,D]));
format_date(Inv) ->
    {error, {invalid_format, Inv}}.

%% integers needs to be alinged with gb_log.hrl
fmt_level(0) ->
    "dbg";
fmt_level(1) ->
    "inf";
fmt_level(2) ->
    "ntc";
fmt_level(3) ->
    "wrn";
fmt_level(4) ->
    "err";
fmt_level(5) ->
    "crt";
fmt_level(6) ->
    "alr";
fmt_level(7) ->
    "eme";
fmt_level(_) ->
    "inf".

fmt_log(LF = #lf{ts=undefined}) ->
    Now = {_, _, MicroSecs} = os:timestamp(),
    {Date, Time} = calendar:now_to_local_time(Now),
    String = io_lib:format(ff(LF#lf.fmt), LF#lf.args),
    Ts = [format_date(Date), " ", 
	  format_time(Time), ".", 
	  io_lib:format("~3.3.0w", [MicroSecs div 1000])],
    Level = fmt_level(LF#lf.level),
    _LogData = io_lib:format("~s ~s [~p:~p] ~p ~s\n", [Ts, Level, LF#lf.mod,
						       LF#lf.line, LF#lf.pid,
						       String]).

ff(FmtStr) ->
    re:replace(FmtStr, "~p", "~100000p", [{return,list}, global]).

do_log(Log) when is_list(Log) ->
    case whereis(?MODULE) of
	undefined ->
	    io:format(Log),
	    ok;
	Pid ->
	    Pid ! {log, Log},
	    ok
    end.

log(LF = #lf{}) ->
    try
	LogLine = fmt_log(LF),
	do_log(LogLine)
    catch _:_ ->
	ok
    end.


ascii_log(S, Data) ->
    NewS = check_wrap(S),
    prim_file:write(NewS#state.filefd, Data),
    NewS.

open_log(Fname) ->
    {ok, _FD} = prim_file:open(Fname, [append]).

check_wrap(S = #state{filefd = FD}) ->
    {ok, Size} = prim_file:position(FD, eof),
    {ok, NewFD} = step_files(S, Size),
    S#state{filefd=NewFD}.

step_files(#state{fsize=MaxSize, filefd=FD}, Size) when Size < MaxSize ->
    {ok, FD};
step_files(S = #state{fname=Fname, nfiles=Nfiles}, _Size) ->
    prim_file:write(S#state.filefd, "wrapping log...\n"),
    prim_file:close(S#state.filefd),
    file:delete(Fname ++ "." ++ integer_to_list(Nfiles)),
    do_step_files(Fname, Nfiles).

do_step_files(Fname, Nfiles) when Nfiles > 1 ->
    file:rename(Fname ++ "." ++ integer_to_list(Nfiles-1),
	        Fname ++ "." ++ integer_to_list(Nfiles)),
    do_step_files(Fname, Nfiles-1);
do_step_files(Fname, 1) ->
    file:rename(Fname,
		Fname ++ ".1"),
    open_log(Fname).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

%% set default config
-define(gb_conf_default, "gb_log.yaml").
init() ->
    RootDir = gb_conf_env:logdir(),
    LogName = ?gb_conf_get(logname, "pundun.log"),
    FSize  = ?gb_conf_get(maxsize, 60) % default 60MB
			     * math:pow(2,20), %% convert to bytes
    NFiles  = ?gb_conf_get(number_of_files, 30),
    RHost   = ?gb_conf_get(remote_host, "127.0.0.1"),
    RPort   = ?gb_conf_get(remote_port, 32000),
    Type   = cast_to_atom(?gb_conf_get(type, ascii)),

    gb_log_oam:load_store_filters_beam(),
    gb_log_oam:load_default_filter(),

    %% receive shutdown messages
    erlang:process_flag(trap_exit, true),

    {ok, Sock} = gen_udp:open(0, [{buffer, trunc(10 * math:pow(2,20))},
				  binary]),

    Fname = filename:join(RootDir, LogName),
    filelib:ensure_dir(Fname),
    {ok, FileFD} = open_log(Fname),

    true = register(?MODULE, self()),
    proc_lib:init_ack({ok, self()}),
    State = #state{type=Type, filefd=FileFD, fname=Fname,
		   fsize = FSize, nfiles = NFiles,
		   udpfd=Sock, remhost=RHost, remport=RPort,
		   node=atom_to_list(node())},

    %% register our error logger event handler
    gb_log_el:add(),

    %% remove default error_logger
    (catch gen_event:delete_handler(error_logger, error_logger, delete)),

    prim_file:write(State#state.filefd, internal_fmt("Logging started.", [])),
    log_loop(State, {0, []}).

log_loop(S, {Thold, Buffer}) when Thold < 10000 ->
    receive
	{log, Data} ->
	    log_loop(S, {Thold + 1, [Data|Buffer]})
    after 10  ->
	    receive
		{log, Data} ->
		    log_loop(S, {Thold + 3000, [Data|Buffer]});
		{'EXIT', _FromPid, shutdown} ->
		    log_data(lists:reverse(Buffer), S),
		    terminate(S)
	    after 3000 ->
		NewS = log_data(lists:reverse(Buffer), S),
		?MODULE:log_loop(NewS, {0, []})
	    end
    end;
log_loop(S, {_,Buffer}) ->
    NewS = log_data(lists:reverse(Buffer), S),
    log_loop(NewS, {0, []}).
	    
log_data([], S) ->
    S;
log_data(Data, S = #state{type=ascii}) ->
    _NewS = ascii_log(S, Data);
log_data(Data, S = #state{type=both}) ->
    gen_udp:send(S#state.udpfd, S#state.remhost, S#state.remport, [S#state.node," ==\n",Data]),
    _NewS = ascii_log(S, Data).

terminate(State) ->
    prim_file:write(State#state.filefd, internal_fmt("Shutting down..", [])),
    gb_log_el:remove(),
    prim_file:close(State#state.filefd),
    exit(shutdown).

internal_fmt(Fmt, Args) ->
    Log = #lf{mod=?MODULE,
	      pid=self(),
	      line=?LINE,
	      fmt=Fmt,
	      args=Args},
    _LogData = fmt_log(Log).

cast_to_atom(List) when is_list(List) ->
    list_to_atom(List);
cast_to_atom(Atom) when is_atom(Atom) ->
    Atom.
