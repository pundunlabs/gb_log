-record(lf, {level, ts, mod, line, fmt, args}).
-define(debug(FMT, Args), 
	    Log = #lf{level=debug,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args},
	    gb_log:log(Log)).

