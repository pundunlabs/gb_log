-record(lf, {level, ts, mod, line, fmt, args}).
-define(debug(FMT, Args), 
	    gb_log:log(#lf{level=debug,
		      mod=?MODULE,
		      line=?LINE,
		      fmt=FMT,
		      args=Args}).

