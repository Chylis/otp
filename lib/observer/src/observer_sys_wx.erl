-module(observer_sys_wx).

-behaviour(wx_object).

-export([start_link/3]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

start_link(Notebook, Panel, Env) ->
    wx:set_env(Env),
    wx_object:start_link(?MODULE, [Notebook, Panel], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, SysPanel]) ->
    
    %% Setup sizers
    SysSizer = wxBoxSizer:new(?wxVERTICAL),
    
    SysNodeSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Node:"}]),
    
    SysLoadSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Load:"}]),
    SysLeftLoadSizer = wxBoxSizer:new(?wxVERTICAL),
    SysMidLoadSizer = wxBoxSizer:new(?wxHORIZONTAL),
    SysRightLoadSizer = wxBoxSizer:new(?wxVERTICAL),
    
    SysMemSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, SysPanel, [{label, "Memory:"}]),
    SysLeftMemSizer = wxBoxSizer:new(?wxVERTICAL),
    SysMidMemSizer = wxBoxSizer:new(?wxHORIZONTAL),
    SysRightMemSizer = wxBoxSizer:new(?wxVERTICAL),
    
    wxSizer:add(SysSizer, SysNodeSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysSizer, SysLoadSizer),%, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysSizer, SysMemSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysLoadSizer, SysLeftLoadSizer),
    wxSizer:add(SysLoadSizer, SysMidLoadSizer),
    wxSizer:add(SysLoadSizer, SysRightLoadSizer),
    
    wxSizer:add(SysMemSizer, SysLeftMemSizer),
    wxSizer:add(SysMemSizer, SysMidMemSizer),
    wxSizer:add(SysMemSizer, SysRightMemSizer),
    
    wxSizer:addSpacer(SysMidLoadSizer, 90),
    wxSizer:addSpacer(SysMidMemSizer, 70),
    
    %%Create labels
    NodeInfo = get_syspage_info(),
    create_info_label(SysPanel, SysNodeSizer, ?OBS_SYS_LOGIC:node_name_str(NodeInfo)),
    
    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's available:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "logical CPU's online:"),
    create_info_label(SysPanel, SysLeftLoadSizer, "existing processes:"),
    NoCpuTxt = create_info_label(SysPanel, SysRightLoadSizer, ?OBS_SYS_LOGIC:no_cpu_str(NodeInfo)),
    NoCpuAvTxt = create_info_label(SysPanel, SysRightLoadSizer, ?OBS_SYS_LOGIC:no_cpu_available_str(NodeInfo)),
    NoCpuOnTxt = create_info_label(SysPanel, SysRightLoadSizer, ?OBS_SYS_LOGIC:no_cpu_online_str(NodeInfo)),
    NoProcsTxt = create_info_label(SysPanel, SysRightLoadSizer, ?OBS_SYS_LOGIC:no_procs_str(NodeInfo)),
    
    create_info_label(SysPanel, SysLeftMemSizer, "total allocated Mb:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb used by processes:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb allocated for processes:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb used by atoms:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb allocated for atoms:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb allocated for binaries:"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb allocated for code"),
    create_info_label(SysPanel, SysLeftMemSizer, "Mb allocated for ETS:"),
    TotAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:tot_alloc_str(NodeInfo)),
    ProcUsedTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:proc_used_str(NodeInfo)),
    ProcAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:proc_alloc_str(NodeInfo)),
    AtomUsedTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:atom_used_str(NodeInfo)),
    AtomAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:atom_alloc_str(NodeInfo)),
    BinaryAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:binary_alloc_str(NodeInfo)),
    CodeAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:code_alloc_str(NodeInfo)),
    EtsAllocTxt = create_info_label(SysPanel, SysRightMemSizer, ?OBS_SYS_LOGIC:ets_alloc_str(NodeInfo)),
    
    %% Create StateRecord
    SysPanelState = #state{name = sys_panel, 
			   ref = SysPanel,
			   children = [#obj{name = no_procs, ref = NoProcsTxt},
				       #obj{name = no_cpu, ref = NoCpuTxt},
				       #obj{name = no_cpu_available, ref = NoCpuAvTxt},
				       #obj{name = no_cpu_online, ref = NoCpuOnTxt},
				       #obj{name = tot_alloc, ref = TotAllocTxt},
				       #obj{name = proc_used, ref = ProcUsedTxt},
				       #obj{name = proc_alloc, ref = ProcAllocTxt},
				       #obj{name = atom_used, ref = AtomUsedTxt},
				       #obj{name = atom_alloc, ref = AtomAllocTxt},
				       #obj{name = binary_alloc, ref = BinaryAllocTxt},
				       #obj{name = code_alloc, ref = CodeAllocTxt},
				       #obj{name = ets_alloc, ref = EtsAllocTxt}]},
    
    
    wxPanel:setSizer(SysPanel, SysSizer),
    erlang:send_after(1000, self(), {update, Notebook}),
    {SysPanel, SysPanelState}.

get_syspage_info() ->
    rpc:call(node(), ?OBS_SYS_LOGIC, node_info, []).

create_info_label(Panel, Sizer, Msg) ->
    WxText = wxStaticText:new(Panel, ?wxID_ANY, Msg),
    wxSizer:add(Sizer, WxText),
    WxText.

update_syspage(Notebook, State) ->
    case ?OBS:check_page_title(Notebook) =:= "System" of
	true ->
	    io:format("Updating syspage...~n"),
	    NodeInfo = get_syspage_info(),
	    TxtLabelList = State#state.children,
	    
	    lists:foreach(fun(X) ->
				  update_info_label(X, NodeInfo)
			  end, 
			  TxtLabelList);
	false ->
	    ok
    end.

update_info_label(#obj{name = Name, ref = WxText}, NodeInfo) ->
    Msg = case Name of
	      no_procs ->
		  ?OBS_SYS_LOGIC:no_procs_str(NodeInfo);
	      no_cpu ->
		  ?OBS_SYS_LOGIC:no_cpu_str(NodeInfo);
	      no_cpu_available ->
		  ?OBS_SYS_LOGIC:no_cpu_available_str(NodeInfo);
	      no_cpu_online ->
		  ?OBS_SYS_LOGIC:no_cpu_online_str(NodeInfo);
	      tot_alloc ->
		  ?OBS_SYS_LOGIC:tot_alloc_str(NodeInfo);
	      proc_used ->
		  ?OBS_SYS_LOGIC:proc_used_str(NodeInfo);
	      proc_alloc ->
		  ?OBS_SYS_LOGIC:proc_alloc_str(NodeInfo);
	      atom_used ->
		  ?OBS_SYS_LOGIC:atom_used_str(NodeInfo);
	      atom_alloc ->
		  ?OBS_SYS_LOGIC:atom_alloc_str(NodeInfo);
	      binary_alloc ->
		  ?OBS_SYS_LOGIC:binary_alloc_str(NodeInfo);
	      code_alloc ->
		  ?OBS_SYS_LOGIC:code_alloc_str(NodeInfo);
	      ets_alloc ->
		  ?OBS_SYS_LOGIC:ets_alloc_str(NodeInfo)
	  end,
    wxStaticText:setLabel(WxText, Msg).


%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({update, Notebook}, State) ->
    update_syspage(Notebook, State),
    erlang:send_after(1000, self(), {update, Notebook}),
    {noreply, State};

handle_info(Info, State) ->
    io:format("~p, ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.
