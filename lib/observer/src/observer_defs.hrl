-define(OBS, observer_wx).
-define(OBS_SYS_LOGIC, observer_sys).
-define(OBS_SYS_WX, observer_sys_wx).

-record(state,
	{
	  name,
	  ref,
	  parent,
	  children,
	  node_name,
	  known_nodes
	}).

-record(obj,
	{
	  name,
	  parent,
	  children,
	  ref
	}).

-record(node_info, {node_name,
		    no_procs, % number of processes
		    no_cpu, % number of logical cpu's
		    no_cpu_available, %number of logical cpu's available
		    no_cpu_online, % number of logical cpu's online
		    tot_alloc, % total memory allocated
		    proc_used, % memory used by processes
		    proc_alloc, % memory alloc by processes,
		    atom_used, % memory used by atoms
		    atom_alloc, % memory allocated by atoms
		    binary_alloc, % memory allocated for binaries
		    code_alloc, % memory allocated by code
		    ets_alloc}).% memory allocated by ets
