%%--------------------------------------------------------------------
%%<copyright>
%% <year>1997-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%-----------------------------------------------------------------

-define(debug_out, false).
-define(SYSFORM,
	" ~-72w~10s~n"
	" Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n"
	"        procs~8w                        processes~8w    code     ~8w~n"
	"        runq ~8w                        atom     ~8w    ets      ~8w~n").

-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 30, 
	       width = 700, height = 340, sort = runtime, tracing = on,
	       %% Other state information
	       out_mod=erltop:output(graphical), out_proc, server, host, tracer, store, 
	       accum_tab, remote}).

-record(trace_options, {send         = true,
			treceive     = true,
			functions    = true,
			events       = true,
			on_1st_spawn = true,
			on_all_spawn = false,
			on_1st_link  = true,
			on_all_link  = false,
			in_window    = true,
			to_file      = false,
			file         = "",
			main_window  = true}).
-record(on_spawn, {checkbox, radio1, radio2}).
-record(on_link, {checkbox, radio1, radio2}).

-record(pid, {window, traced}).

-record(create_menu, {id,
		      text,
		      type = append,
		      check = true}).
