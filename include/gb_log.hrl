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

-record(lf, {level, ts, mod, line, fmt, args}).

-define(debug_level,	0).
-define(info_level,	1).
-define(notice_level,	2).
-define(warning_level,	3).
-define(error_level,	4).
-define(critical_level,	5).
-define(alert_level,	6).
-define(emergency_level,7).

-define(log_boiler_plate(Level, FMT, Args),
	    gb_log_filter:filter(
		  #lf{level=Level,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args}), ok).

-define(debug(FMT, Args), ?log_boiler_plate(?debug_level, FMT, Args)).
-define(debug(FMT), ?debug(FMT, [])).

-define(info(FMT, Args), ?log_boiler_plate(?info_level, FMT, Args)).
-define(info(FMT), ?info(FMT, [])).

-define(notice(FMT, Args), ?log_boiler_plate(?notice_level, FMT, Args)).
-define(notice(FMT), ?notice(FMT, [])).

-define(warning(FMT, Args), ?log_boiler_plate(?warning_level, FMT, Args)).
-define(warning(FMT), ?warning(FMT, [])).

-define(error(FMT, Args), ?log_boiler_plate(?error_level, FMT, Args)).
-define(error(FMT), ?error(FMT, [])).

-define(critical(FMT, Args), ?log_boiler_plate(?critical_level, FMT, Args)).
-define(critical(FMT), ?critical(FMT, [])).

-define(alert(FMT, Args), ?log_boiler_plate(?alert_level, FMT, Args)).
-define(alert(FMT), ?alert(FMT, [])).

-define(emergency(FMT, Args), ?log_boiler_plate(?emergency_level, FMT, Args)).
-define(emergency(FMT), ?emergency(FMT, [])).
