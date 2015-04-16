-record(lf, {level, ts, mod, line, fmt, args}).
-define(debug(FMT, Args),
	    gb_log_filter:filter(#lf{level=?debug_level,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args})).

-define(warning(FMT, Args),
	    gb_log_filter:filter(#lf{level=?warning_level,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args})).

-define(fatal(FMT, Args),
	    gb_log_filter:filter(#lf{level=?fatal_level,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args})).


-define(debug_level,	1).
-define(verbose_level,	2).
-define(trace_level,	3).
-define(normal_level,	4).
-define(warning_level,	5).
-define(fatal_level,	6).
