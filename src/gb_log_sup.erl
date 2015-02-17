%%%-------------------------------------------------------------------
% author jonas.falkevik@mobilearts.com
%
%%%-------------------------------------------------------------------
-module(gb_log_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor for gb_log
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
					  ignore |
					  {error, Error :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init([]) ->
    AChild = {'gb_log', {'gb_log', start_link, []},
	      _Restart = permanent, 
	      _Shutdown = 2000, 
	      _Type = worker, ['gb_log']},

    {ok, {_SupFlags = 
	   {_Strategy = one_for_one, 
	    _MaxRestarts = 1000,
	    _MaxSecondsBetweenRestarts = 20}, [AChild]}}.

