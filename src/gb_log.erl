-module(gb_log).
-compile(export_all).
-export([init/0]).
	 
-record(state, {type, filefd, udpfd, remhost, remport, node,
		fname, fsize, nfiles}).
-include("gb_log.hrl").
-include_lib("kernel/include/file.hrl").

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

fmt_log(LF = #lf{ts=undefined}) ->
    Now = {_, _, MicroSecs} = os:timestamp(),
    {Date, Time} = calendar:now_to_local_time(Now),
    String = io_lib:format(LF#lf.fmt, LF#lf.args),
    Ts = [format_date(Date), " ", 
	  format_time(Time), ".", 
	  io_lib:format("~3.3.0w", [MicroSecs div 1000])],
    _LogData = io_lib:format("~s [~p:~p] ~s\n", [Ts, LF#lf.mod, LF#lf.line, String]).

do_log(Log) when is_binary(Log) ->
    ?MODULE ! {log, Log},
    ok;

do_log(Log0) when is_list(Log0) ->
    Log = list_to_binary(Log0),
    ?MODULE ! {log, Log},
    ok.

%% TODO: this should be moved out to log filter
%% for now; log everything independent of log level
log(LF = #lf{}) ->
    LogLine = fmt_log(LF),
    do_log(LogLine).

ascii_log(S, Data) ->
    NewS = check_wrap(S),
    prim_file:write(NewS#state.filefd, Data),
    NewS.

close_log(#state{filefd = FileFD}) ->
    file:close(FileFD),
    ok.

open_log(Fname) ->
    {ok, _FD} = prim_file:open(Fname, [append]).

check_wrap(S = #state{filefd = FD}) ->
    {ok, Size} = prim_file:position(FD, eof),
    {ok, NewFD} = step_files(S, Size),
    S#state{filefd=NewFD}.

step_files(#state{fsize=MaxSize, filefd=FD}, Size) when Size < MaxSize ->
    {ok, FD};
step_files(#state{fname=Fname, nfiles=Nfiles}, _Size) ->
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

init() ->
    {ok, Sock} = gen_udp:open(0),
    RootDir = "log", %gb_conf:get_param("gb_conf.json", "rootdir"),
    LogName = "local.pundun.log", %gb_conf:get_param("gb_log.json", "logname"),
    FSize = 60 * 1024 * 1024, % gb_conf:get_param("gb_log.json", "maxsize"),
    NFiles = 20, %gb_conf:get_param("gb_log.json", "number_of_files"),
    Fname = filename:join(RootDir, LogName),
    {ok, FileFD} = open_log(Fname),
    RHost = "127.0.0.1", % gb_conf:get_param("gb_log.json", "remote_host"),
    RPort = 32000,       % gb_conf:get_param("gb_log.json", "remote_port"),
    
    true = register(?MODULE, self()),
    proc_lib:init_ack({ok, self()}),
    State = #state{type=both, filefd=FileFD, fname=Fname,
		   fsize = FSize, nfiles = NFiles,
		   udpfd=Sock, remhost=RHost, remport=RPort, 
		   node=atom_to_list(node())},
    log_loop(State, {0, []}).

log_loop(S, {Thold, Buffer}) when Thold < 10000 ->
    receive
	{log, Data} ->
	    log_loop(S, {Thold + 1, [Data|Buffer]})
    after 10  ->
	    receive {log, Data} ->
		log_loop(S, {Thold + 3000, [Data|Buffer]})
	    after 3000 ->
		NewS = log_data(lists:reverse(Buffer), S),
		?MODULE:log_loop(NewS, {0, []})
	    end
    end;
log_loop(S, {_,Buffer}) ->
    NewS = log_data(lists:reverse(Buffer), S),
    log_loop(NewS, {0, []}).
	    
log_data(Data, S = #state{type=both}) ->
    gen_udp:send(S#state.udpfd, S#state.remhost, S#state.remport, Data),
    _NewS = ascii_log(S, Data).

