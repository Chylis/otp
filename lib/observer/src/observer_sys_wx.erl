-module(observer_sys_wx).

-behaviour(wx_object).

-export([start_link/3]).
%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2]).

-include_lib("wx/include/wx.hrl").

%% Defines
-define(OBS_SYS_LOGIC, observer_sys).
-define(OBS, observer_wx).

-define(ID_REFRESH, 3).


%% Records
-record(sys_wx_state,
	{panel,
	 menubar,
	 parent_notebook,
	 no_procs,
	 no_cpu,
	 no_cpu_available,
	 no_cpu_online,
	 tot_alloc,
	 proc_used,
	 proc_alloc,
	 atom_used,
	 atom_alloc,
	 binary_alloc,
	 code_alloc,
	 ets_alloc,
	 node_label,
	 node}).

-record(create_menu, 
	{id,
	 text,
	 type = append,
	 check = true
	}).

start_link(Notebook, Env, MenuBar) ->
    wx:set_env(Env),
    wx_object:start_link(?MODULE, [Notebook, MenuBar], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, MenuBar]) ->
    SysPanel = wxPanel:new(Notebook, []),
    
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
    wxSizer:add(SysSizer, SysLoadSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysSizer, SysMemSizer, [{flag, ?wxEXPAND}]),
    wxSizer:add(SysLoadSizer, SysLeftLoadSizer),
    wxSizer:add(SysLoadSizer, SysMidLoadSizer),
    wxSizer:add(SysLoadSizer, SysRightLoadSizer),
    
    wxSizer:add(SysMemSizer, SysLeftMemSizer),
    wxSizer:add(SysMemSizer, SysMidMemSizer),
    wxSizer:add(SysMemSizer, SysRightMemSizer),
    
    wxSizer:addSpacer(SysMidLoadSizer, 90),
    wxSizer:addSpacer(SysMidMemSizer, 70),
    
    %% Create labels
    NodeInfo = get_syspage_info(node()),
    NodeLabel = create_info_label(SysPanel, SysNodeSizer, ?OBS_SYS_LOGIC:node_name_str(NodeInfo)),
    
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
    SysPanelState = #sys_wx_state{
      panel = SysPanel,
      menubar = MenuBar,
      parent_notebook = Notebook,
      node_label = NodeLabel, 
      no_procs = NoProcsTxt,
      no_cpu = NoCpuTxt,
      no_cpu_available = NoCpuAvTxt,
      no_cpu_online= NoCpuOnTxt,
      tot_alloc = TotAllocTxt,
      proc_used = ProcUsedTxt,
      proc_alloc = ProcAllocTxt,
      atom_used = AtomUsedTxt,
      atom_alloc = AtomAllocTxt,
      binary_alloc = BinaryAllocTxt,
      code_alloc = CodeAllocTxt,
      ets_alloc = EtsAllocTxt,
      node = node()
     },
    
    wxPanel:setSizer(SysPanel, SysSizer),
    create_sys_menu(MenuBar),
    erlang:send_after(3000, self(), {update, Notebook}),
    {SysPanel, SysPanelState}.

get_syspage_info(Node) ->
    rpc:call(Node, ?OBS_SYS_LOGIC, node_info, []).

create_info_label(Panel, Sizer, Msg) ->
    WxText = wxStaticText:new(Panel, ?wxID_ANY, Msg),
    wxSizer:add(Sizer, WxText),
    WxText.


create_menu(MenuItems, Name, MenuBar) ->
    Menu = wxMenu:new(),
    lists:foreach(fun(Record) ->
			  create_menu_item(Record, Menu)
		  end,
		  MenuItems),
    wxMenuBar:append(MenuBar, Menu, Name),
    Menu.

create_menu_item(#create_menu{id = Id, text = Text, type = Type, check = Check}, Menu) ->
    case Type of
	append ->
	    wxMenu:append(Menu, Id, Text);
	appendCheck ->
	    wxMenu:appendCheckItem(Menu, Id, Text),
	    wxMenu:check(Menu, Id, Check);
	separator ->
	    wxMenu:appendSeparator(Menu)
    end.


create_sys_menu(MenuBar) ->
    clear_menu_bar(MenuBar),
    create_menu([#create_menu{id = ?ID_REFRESH, text = "&Refresh"}], "View", MenuBar).
    
clear_menu_bar(MenuBar) ->
    Count = wxMenuBar:getMenuCount(MenuBar),
    remove_menu_item(MenuBar, Count).

remove_menu_item(_MenuBar, 2) ->
    ok;
remove_menu_item(MenuBar, Item) ->
    wxMenuBar:remove(MenuBar, Item),
    remove_menu_item(MenuBar, Item-1).

update_syspage(Notebook, State) ->
    case ?OBS:check_page_title(Notebook) =:= "System" of
	true ->
	    io:format("Updating syspage for node ~p~n", [State#sys_wx_state.node]),
	    
	    update_info_label(State#sys_wx_state.node, node_label, State#sys_wx_state.node_label),
	    update_info_label(State#sys_wx_state.node, no_procs, State#sys_wx_state.no_procs),
	    update_info_label(State#sys_wx_state.node, no_procs, State#sys_wx_state.no_procs),
	    update_info_label(State#sys_wx_state.node, no_cpu, State#sys_wx_state.no_cpu),
	    update_info_label(State#sys_wx_state.node, no_cpu_available, State#sys_wx_state.no_cpu_available),
	    update_info_label(State#sys_wx_state.node, no_cpu_online, State#sys_wx_state.no_cpu_online),
	    update_info_label(State#sys_wx_state.node, tot_alloc, State#sys_wx_state.tot_alloc),
	    update_info_label(State#sys_wx_state.node, proc_used, State#sys_wx_state.proc_used),
	    update_info_label(State#sys_wx_state.node, proc_alloc, State#sys_wx_state.proc_alloc),
	    update_info_label(State#sys_wx_state.node, atom_used, State#sys_wx_state.atom_used),
	    update_info_label(State#sys_wx_state.node, atom_alloc, State#sys_wx_state.atom_alloc),
	    update_info_label(State#sys_wx_state.node, binary_alloc, State#sys_wx_state.binary_alloc),
	    update_info_label(State#sys_wx_state.node, code_alloc, State#sys_wx_state.code_alloc),
	    update_info_label(State#sys_wx_state.node, ets_alloc, State#sys_wx_state.ets_alloc);
	
	false ->
	    ok
    end.

update_info_label(Node, Name, WxText) ->
    NodeInfo = get_syspage_info(Node),
    Msg = case Name of
	      node_label ->
		  ?OBS_SYS_LOGIC:node_name_str(NodeInfo);
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
    erlang:send_after(3000, self(), {update, Notebook}),
    {noreply, State};

handle_info({node, Node}, State) ->
    UpdState = State#sys_wx_state{node = Node},
    update_syspage(State#sys_wx_state.parent_notebook, UpdState),
    {noreply, UpdState};

handle_info(Info, State) ->
    io:format("~p, ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_event(#wx{id = ?ID_REFRESH, event = #wxCommand{type = command_menu_selected}}, State) ->
    io:format("~p:~p, Klickade på refresh~n", [?MODULE, ?LINE]),
    update_syspage(State#sys_wx_state.parent_notebook, State),
    {noreply, State};

handle_event(#wx{event = #wxNotebook{type = command_notebook_page_changed}}, State) ->
    create_sys_menu(State#sys_wx_state.menubar),
    {noreply, State};

handle_event(Event, State) ->
    io:format("handle event ~p\n", [Event]),
    {noreply, State}.
