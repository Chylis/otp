%%%-------------------------------------------------------------------
-module(observer_traceoptions_wx).

-include_lib("wx/include/wx.hrl").
-include("erltop_defs.hrl").

-export([start/6]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-record(traceopts_state, {
	  parent,
	  frame,
	  tree,
	  module_infobox_open = false,
	  match_specs, % [ #match_spec ]
	  checked_funcs = [],
	  traced_funcs,  % Key =:= Module::atom, Value =:= [ #traced_func ]
	  trace_options}).
			  

-record(boxes, {send, 'receive', functions, events,
		on_spawn, on_link, all_spawn, all_link}).

-record(match_spec, {alias,
		     term_ms,
		     str_ms = [],
		     fun2ms}).

-record(traced_func, {func_name, %atom
		      arity, %integer
		      match_spec = #match_spec{}}). % #match_spec



-define(SELECT, 10).
-define(SELECT_ALL, 11).
-define(FUN2MS, 12).
-define(ADD_MS_BTN, 13).
-define(ADD_MS_ALIAS_BTN, 14).
-define(FRAME_TRACEOPTS, 15).


start(ParentFrame, ParentPid, Node, TraceOpts, TracedFuncs, MatchSpecs) ->
    wx_object:start(?MODULE, [ParentFrame, ParentPid, Node, TraceOpts, 
			      TracedFuncs, MatchSpecs], []).

init([ParentFrame, ParentPid, Node, TraceOpts, TracedFuncs, MatchSpecs]) ->
    {Frame, Tree} = setup(ParentFrame, Node, TraceOpts, TracedFuncs, MatchSpecs),
    {Frame, #traceopts_state{
       parent = ParentPid,
       frame = Frame,
       tree = Tree,
       module_infobox_open = false,
       match_specs = MatchSpecs,
       traced_funcs = TracedFuncs,
       trace_options = TraceOpts}}.

setup(ParentFrame, Node, TraceOpts, TracedFuncs, MatchSpecs) ->

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup main window
    
    Frame = wxFrame:new(ParentFrame, ?FRAME_TRACEOPTS, "Trace options",
			[{size, {400, 500}}]),
    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Notebook = wxNotebook:new(Panel, ?wxID_ANY),
    Modules = get_modules(Node),
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup option page
    
    OptPanel = wxPanel:new(Notebook),
    OptMainSz = wxBoxSizer:new(?wxVERTICAL),
    TopSz = wxBoxSizer:new(?wxHORIZONTAL),
    TopLeftSz = wxStaticBoxSizer:new(?wxVERTICAL, OptPanel, 
				     [{label, "Tracing options"}]),
    TopRightSz = wxStaticBoxSizer:new(?wxVERTICAL, OptPanel, 
				      [{label, "Inheritance options:"}]),

    SendBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace send", []), 
    check_box(SendBox, TraceOpts#trace_options.send),
    RecBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace receive", []),
    check_box(RecBox, TraceOpts#trace_options.treceive),
    FuncBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace functions", []),
    check_box(FuncBox, TraceOpts#trace_options.functions),
    EventBox = wxCheckBox:new(OptPanel, ?wxID_ANY, "Trace events", []),
    check_box(EventBox, TraceOpts#trace_options.events),

    {SpawnBox, SpwnAllRadio, SpwnFirstRadio} = 
	optionpage_top_right(OptPanel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "spawn"),
    {LinkBox, LinkAllRadio, LinkFirstRadio} = 
	optionpage_top_right(OptPanel, TopRightSz, [{flag, ?wxBOTTOM},{border, 5}], "link"),
    SpawnBool = TraceOpts#trace_options.on_all_spawn or TraceOpts#trace_options.on_1st_spawn,
    LinkBool = TraceOpts#trace_options.on_all_link or TraceOpts#trace_options.on_1st_link,
    check_box(SpawnBox, SpawnBool),
    check_box(LinkBox,  LinkBool),
    enable({SpawnBox, SpwnAllRadio, SpwnFirstRadio}),
    enable({LinkBox, LinkAllRadio, LinkFirstRadio}),
    wxRadioButton:setValue(SpwnAllRadio, TraceOpts#trace_options.on_all_spawn),
    wxRadioButton:setValue(SpwnFirstRadio, TraceOpts#trace_options.on_1st_spawn),
    wxRadioButton:setValue(LinkAllRadio, TraceOpts#trace_options.on_all_link),
    wxRadioButton:setValue(LinkFirstRadio, TraceOpts#trace_options.on_1st_link),
    
    wxSizer:add(TopLeftSz, SendBox, []),
    wxSizer:add(TopLeftSz, RecBox, []),
    wxSizer:add(TopLeftSz, FuncBox, []),
    wxSizer:add(TopLeftSz, EventBox, []),
    wxSizer:add(TopLeftSz, 150, -1),

    wxSizer:add(TopSz, TopLeftSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(TopSz, TopRightSz,[{flag, ?wxEXPAND}]),
    wxSizer:add(OptMainSz, TopSz, []),      
    wxWindow:setSizer(OptPanel, OptMainSz),
    wxNotebook:addPage(Notebook, OptPanel, "Tracing"),
    
%%%%%%%%%%%%%%%%%%%%%%%% Setup trace function page

    FuncPanel = wxPanel:new(Notebook),
    FuncMainSz = wxBoxSizer:new(?wxVERTICAL),
    ModuleSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Select module"}]),
    TreeSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Selected functions"}]),
    
    AllModules = atomlist_to_stringlist(Modules),
    ModuleTxtCtrl = wxTextCtrl:new(FuncPanel, ?wxID_ANY),
    ModuleListBox = wxListBox:new(FuncPanel, ?wxID_ANY, [{choices, AllModules}, {style, ?wxLB_SINGLE}]),
    TreeCtrl = wxTreeCtrl:new(FuncPanel),
    wxTreeCtrl:addRoot(TreeCtrl, atom_to_list(Node)),
    update_tree(TreeCtrl, TracedFuncs), 
    
    wxTextCtrl:connect(ModuleTxtCtrl, command_text_updated, 
		       [{userData, {trace_funcs, AllModules, ModuleListBox}}]),
    wxListBox:connect(ModuleListBox, command_listbox_doubleclicked, 
		      [{userData, trace_funcs}]),
    wxTreeCtrl:connect(TreeCtrl, command_tree_item_activated),

    wxSizer:add(ModuleSz, ModuleTxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(ModuleSz, ModuleListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(TreeSz, TreeCtrl, [{flag, ?wxEXPAND},{proportion, 1}]),
    wxSizer:add(FuncMainSz, ModuleSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(FuncMainSz, TreeSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(FuncPanel, FuncMainSz),
    wxNotebook:addPage(Notebook, FuncPanel, "Functions"),

    
%%%%%%%%%%%%%%%%%%% Setup match specification page
    
    {MatchPanel, _, _} = create_matchspec_page(Notebook, MatchSpecs),
    wxNotebook:addPage(Notebook, MatchPanel, "Match Specs"),


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Setup Dialog
    
    wxSizer:add(MainSz, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),
    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, OKBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    wxSizer:add(MainSz, DialogBtnSz),
    wxWindow:setSizer(Panel, MainSz),

    Boxes = #boxes{send = SendBox,
		   'receive' = RecBox,
		   functions = FuncBox,
		   events = EventBox,
		   on_spawn = #on_spawn{checkbox = SpawnBox,
					all_spawn = SpwnAllRadio,
					first_spawn = SpwnFirstRadio},
		   all_spawn = SpwnAllRadio,
		   on_link = #on_link{checkbox = LinkBox,
				      all_link = LinkAllRadio,
				      first_link = LinkFirstRadio},
		   all_link = LinkAllRadio},

    wxButton:connect(OKBtn, command_button_clicked, [{userData, {trace_options, Boxes}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, trace_options}]),
    wxFrame:connect(Frame, close_window, [{userData, trace_options}]),
    wxFrame:show(Frame),
    {Frame, TreeCtrl}.

get_modules(Node) ->
    lists:sort([Module || {Module, _} <- rpc:call(Node, code, all_loaded, [])]).


optionpage_top_right(Panel, TopRightSz, Options, Text) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Inherit on " ++ Text, []),
    %% wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED),
    RadioSz = wxBoxSizer:new(?wxVERTICAL),
    Radio1 = wxRadioButton:new(Panel, ?wxID_ANY, "All " ++ Text, [{style, ?wxRB_GROUP}]),
    Radio2 = wxRadioButton:new(Panel, ?wxID_ANY, "First " ++ Text ++ " only", []),
    wxSizer:add(Sizer, ChkBox, []),
    wxSizer:add(RadioSz, Radio1, []),
    wxSizer:add(RadioSz, Radio2, []),
    wxSizer:add(Sizer, RadioSz, [{flag, ?wxLEFT},{border, 20}]),
    wxSizer:add(TopRightSz, Sizer, Options),
    wxCheckBox:connect(ChkBox, command_checkbox_clicked, [{userData, {ChkBox, Radio1, Radio2}}]),
    {ChkBox, Radio1, Radio2}.


read_trace_boxes(ChkBoxes = #boxes{on_spawn = OnSpawn, on_link = OnLink}, Options) ->
    {On1stSpawn2, OnAllSpawn2} =
	case wxCheckBox:isChecked(OnSpawn#on_spawn.checkbox) of
	    true ->
		OnAllSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.all_spawn),
		On1stSpawn = wxRadioButton:getValue(OnSpawn#on_spawn.first_spawn),
		{On1stSpawn, OnAllSpawn};
	    false ->
		{false, false}
	end,
    {On1stLink2, OnAllLink2} =
	case wxCheckBox:isChecked(OnLink#on_link.checkbox) of
	    true ->
		OnAllLink = wxRadioButton:getValue(OnLink#on_link.all_link),
		On1stLink = wxRadioButton:getValue(OnLink#on_link.first_link),
		{On1stLink, OnAllLink};
	    false ->
		{false, false}
	end,
    Options#trace_options{send = wxCheckBox:isChecked(ChkBoxes#boxes.send),
			  treceive = wxCheckBox:isChecked(ChkBoxes#boxes.'receive'),
			  functions = wxCheckBox:isChecked(ChkBoxes#boxes.functions),
			  events = wxCheckBox:isChecked(ChkBoxes#boxes.events),
			  on_all_spawn = OnAllSpawn2,
			  on_1st_spawn = On1stSpawn2,
			  on_all_link = OnAllLink2,
			  on_1st_link = On1stLink2}.


create_styled_txtctrl(Parent) ->
    FixedFont = wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxNORMAL,[]),
    Ed = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Ed),
    wxStyledTextCtrl:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Ed, 1, ?wxSTC_MARGIN_NUMBER),
    wxStyledTextCtrl:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Ed, false),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}],
    SetStyle = fun({Style, Color}) ->
		       wxStyledTextCtrl:styleSetFont(Ed, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    wxStyledTextCtrl:setKeyWords(Ed, 0, keyWords()),
    Ed.


keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).


enable({CheckBox, AllRadio, FirstRadio}) ->
    case wxCheckBox:isChecked(CheckBox) of
	false ->
	    wxWindow:disable(AllRadio),
	    wxWindow:disable(FirstRadio);
	true ->
	    wxWindow:enable(AllRadio),
	    wxWindow:enable(FirstRadio)
    end.


check_box(ChkBox, Bool) ->
    case Bool of
	true ->
	    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED);
	false ->
	    ignore
    end.




parse_record_function_names(RecordList) ->
    StrList = [atom_to_list(FName) ++ integer_to_list(Arity)
	       || #traced_func{func_name = FName, arity = Arity} <- RecordList],
    parse_function_names(StrList, []).

parse_function_names(Choices) ->
    StrList = [atom_to_list(Name) ++ integer_to_list(Arity) || {Name, Arity} <- Choices],
    parse_function_names(StrList, []).

parse_function_names([], Acc) ->
    lists:reverse(Acc);
parse_function_names([ [H1|T1] = Head | Tail ], Acc) ->
    Parsed = case is_inner_function(H1, T1) of
		 fun_ ->
		     "Fun: " ++ Head;
		 lc ->
		     "List comprehension: " ++ Head;
		 lbc ->
		     "Bit comprehension: " ++ Head;
		 _ ->
		     Head
	     end,
    parse_function_names(Tail, [Parsed | Acc]).


is_inner_function(Head, Tail) when Head =:= []; Tail =:= [] ->
    false;
is_inner_function($', Tail) ->
    [H|T] = Tail,
    is_inner_function(H, T);
is_inner_function($-, Tail) -> 
    [_|T] = lists:dropwhile(fun(Elem) -> Elem =/= $- end, Tail),
    check_innerfunc_sort(T);
is_inner_function(_, _) ->
    false.

check_innerfunc_sort(List) ->
    Fun = lists:prefix("fun-", List),
    Lc = lists:prefix("lc$^", List),
    Lb = lists:prefix("lbc$^", List),   
    if  Fun =:= true -> fun_;
	Lc =:= true -> lc;
	Lb =:= true -> lbc;
	true ->
	    false
    end.

show_ms_in_savedlistbox(MatchSpecList) ->
    MsOrAlias = fun(#match_spec{alias = A, str_ms = M}) ->
			case A of
			    undefined ->
				M;
			    _ ->
				A
			end
		end,
    [MsOrAlias(X) || X <- MatchSpecList].




find_and_format_ms(Ms, [ #match_spec{str_ms = Spec, alias = Alias, fun2ms = Fun} | T ]) ->
    case (Ms =:= Spec) or (Ms =:= Alias) of
	true ->
	    case Fun of
		undefined ->
		    Spec;
		Fun ->
		    Fun
	    end;
	false ->
	    find_and_format_ms(Ms, T)
    end.

find_ms([], _) ->
    [];
find_ms(Str, [ #match_spec{str_ms = Spec, alias = Alias} = MS | T ]) ->
    case (Str =:= Spec) or (Str =:= Alias) of
	true ->
	    MS;
	false ->
	    find_ms(Str, T)
    end.

apply_matchspec(MatchSpec, TracedDict, root) ->
    UpdateMS = fun(_Key, RecordList) ->
		       [X#traced_func{match_spec = MatchSpec} || X <- RecordList]
	       end,
    dict:map(UpdateMS, TracedDict);
apply_matchspec(MatchSpec, TracedDict, {module, Module}) ->
    RecordList = dict:fetch(Module, TracedDict),
    RecordList2 = [X#traced_func{match_spec = MatchSpec} || X <- RecordList],
    dict:store(Module, RecordList2, TracedDict);
apply_matchspec(MatchSpec, TracedDict, {function, Module, Function, Arity}) ->
    RecordList = dict:fetch(Module, TracedDict),
    [OldFunc] = [X || #traced_func{func_name = Name,
				   arity = A} = X <- RecordList,
		      Name =:= Function, A =:= Arity],
    NewFunc = OldFunc#traced_func{match_spec = MatchSpec},

    RecordList2 = [NewFunc | [X || #traced_func{func_name = Name,
						arity = A} = X <- RecordList, 
				   (Name =/= Function) or (A =/= Arity)]],
    dict:store(Module, RecordList2, TracedDict).

create_matchspec_page(Parent, MatchSpecs) ->
    Panel = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TxtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Match specification:"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    SavedSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Saved match specifications:"}]),

    TxtCtrl = create_styled_txtctrl(Panel),
    wxSizer:add(TxtSz, TxtCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    AddMsBtn = wxButton:new(Panel, ?ADD_MS_BTN, [{label, "Add"}]),
    AddMsAliasBtn = wxButton:new(Panel, ?ADD_MS_ALIAS_BTN, [{label, "Add with alias"}]),
    Fun2MSBtn = wxButton:new(Panel, ?FUN2MS, [{label, "Fun2ms"}]),
    wxSizer:add(BtnSz, AddMsBtn),
    wxSizer:add(BtnSz, AddMsAliasBtn),
    wxSizer:add(BtnSz, Fun2MSBtn),

    Choices = show_ms_in_savedlistbox(MatchSpecs),
    SavedMSListBox = wxListBox:new(Panel, ?wxID_ANY, [{choices, Choices}]),
    wxSizer:add(SavedSz, SavedMSListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxButton:connect(AddMsBtn, command_button_clicked, [{userData, {TxtCtrl, SavedMSListBox}}]),
    wxButton:connect(AddMsAliasBtn, command_button_clicked, [{userData, {Panel, TxtCtrl, SavedMSListBox}}]),
    wxButton:connect(Fun2MSBtn, command_button_clicked),
    wxListBox:connect(SavedMSListBox, command_listbox_selected, [{userData, {trace_matchspec, TxtCtrl}}]),
    wxSizer:add(MainSz, TxtSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSz, BtnSz),
    wxSizer:add(MainSz, SavedSz, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxWindow:setSizer(Panel, MainSz),
    {Panel, MainSz, SavedMSListBox}.



update_tree(Tree, Dict) ->
    RootId = wxTreeCtrl:getRootItem(Tree),
    wxTreeCtrl:deleteChildren(Tree, RootId),
    
    FillTree = fun(KeyAtom, RecordList, acc_in) ->
		       ParsedList = parse_record_function_names(RecordList),
		       Module = wxTreeCtrl:appendItem(Tree, RootId, atom_to_list(KeyAtom)),
		       lists:foldl(fun(#traced_func{func_name = FName, arity = Arity}, N) ->
					   FNameStr = lists:nth(N, ParsedList),
					   wxTreeCtrl:appendItem(Tree, Module, FNameStr, [{data, {KeyAtom, FName, Arity}}]),
					   N+1
				   end,
				   1, RecordList),
		       wxTreeCtrl:sortChildren(Tree, Module),
		       acc_in
	       end,
    dict:fold(FillTree, acc_in, Dict),
    wxTreeCtrl:sortChildren(Tree, RootId),
    wxTreeCtrl:expand(Tree, RootId).




create_module_infobox(Parent, ModuleName, TracedDict) ->
    Module = list_to_atom(ModuleName),
    Value = dict:find(Module, TracedDict),
    TracedModRecs = 
	case Value of
	    {ok, V} ->
		V;
	    error ->
		[]
	end,
    Functions = Module:module_info(functions),
    Choices = lists:sort([{Name, Arity} || {Name, Arity} <- Functions, not(erl_internal:guard_bif(Name, Arity))]),
    ParsedChoices = parse_function_names(Choices),

    Dialog = wxDialog:new(Parent, ?wxID_ANY, ModuleName),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    SelBtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY),
    SelBtn = wxButton:new(Panel, ?SELECT, [{label, "Select"}]),
    DeSelBtn = wxButton:new(Panel, ?SELECT, [{label, "Deselect"}]),
    SelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Deselect all"}]),
    CheckListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{choices, ParsedChoices}, {style, ?wxLB_EXTENDED}]),
    Indices = find_index(TracedModRecs, Choices),
    lists:foreach(fun(X) ->  wxCheckListBox:check(CheckListBox, X) end, Indices),
    Selections = [wxControlWithItems:getString(CheckListBox, I) || I <- Indices],

    OKBtn = wxButton:new(Panel, ?wxID_OK, []),
    CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, OKBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),

    wxSizer:add(SelBtnSz, SelBtn),
    wxSizer:add(SelBtnSz, DeSelBtn),
    wxSizer:add(SelBtnSz, SelAllBtn),
    wxSizer:add(SelBtnSz, DeSelAllBtn),
    wxSizer:add(MainSz, TxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, CheckListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSz, SelBtnSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, DialogBtnSz),
    wxWindow:setSizer(Panel, MainSz),

    wxButton:connect(SelBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, {true, CheckListBox}}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, {false, CheckListBox}}]),
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {module_infobox, Dialog, Module, ParsedChoices, Choices}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, {module_infobox, Dialog}}]),
    wxTextCtrl:connect(TxtCtrl, command_text_updated, [{userData, {module_infobox, ParsedChoices, CheckListBox}}]),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
    wxDialog:connect(Dialog, close_window, [{userData, {module_infobox, Dialog}}]),
    wxDialog:show(Dialog),
    Selections.

get_selections(Selections, FunctionList) ->
    get_selections(Selections, FunctionList, []).
get_selections([], _, Acc) ->
    Acc;
get_selections([Int|T], FuncList, Acc) ->
    get_selections(T, FuncList, [lists:nth(Int, FuncList) | Acc]).

find_index(Selections, FunctionList) ->
    find_index(Selections, FunctionList, 1, []).
find_index(Selections, FunctionList, N, Acc) when N > length(FunctionList); Selections =:= [] ->
    Acc;

find_index([#traced_func{func_name = Name, arity = Arity} |STail] = Selections, 
	   FunctionList, N, Acc) ->
    {Fname, A} = lists:nth(N, FunctionList),
    case (Fname =:= Name) and (A =:= Arity) of
	true ->
	    find_index(STail, FunctionList, 1, [N-1|Acc]);
	false ->
	    find_index(Selections, FunctionList, N+1, Acc)
    end;

find_index([Sel|STail] = Selections, FunctionList, N, Acc) when is_list(Sel) ->
    case lists:nth(N, FunctionList) =:= Sel of
	true ->
	    find_index(STail, FunctionList, 1, [N-1|Acc]);
	false ->
	    find_index(Selections, FunctionList, N+1, Acc)
    end.





atomlist_to_stringlist(Modules) ->
    [atom_to_list(X) || X <- Modules].



check_correct_MS(String) ->
    Tokens = case lists:last(String) =:= $. of
		 true ->
    		     {ok, T, _} = erl_scan:string(String),
		     T;
		 false ->
		     {ok, T, _} = erl_scan:string(String ++ "."),
		     T
	     end,
    
    try erl_parse:parse_term(Tokens) of
	{ok, Term} ->
	    case check_correct_ms_format(Term) of
		true ->
		    {true, Term};
		false ->
		    false
	    end;
	
	_ ->
	    false
    catch
	_ ->
	    false
		
    end.


check_correct_ms_format([H|T] = Term) when length(Term) >= 1,
					is_tuple(H), size(H) =:= 3->
    N = length(T),
    check_correct_ms_format(T, N);
check_correct_ms_format(_Other) ->
    false.

check_correct_ms_format([], 0) ->
    true;
check_correct_ms_format([H|T], N) when is_tuple(H), size(H) =:= 3 ->
    check_correct_ms_format(T, N-1);
check_correct_ms_format(_Other, _) ->
    false.
    


    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Trace option window

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% All pages

handle_event(#wx{id = ?wxID_OK,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {trace_options, Boxes}}, 
	     #traceopts_state{trace_options = TraceOpts, 
			      match_specs = MatchSpecs,
			      traced_funcs = TracedFuncs,
			      parent = Parent} = State) ->
    UpdTraceOpts = wx:batch(fun() ->
				    read_trace_boxes(Boxes, TraceOpts)
			    end),
    Parent ! {updated_traceopts,
	      UpdTraceOpts,
	      MatchSpecs,
	      TracedFuncs},
    {stop, shutdown, State};

handle_event(#wx{id = ?wxID_CANCEL,
		 event = #wxCommand{type = command_button_clicked},
		userData = trace_options},
	     #traceopts_state{parent = Parent} = State) ->
    Parent ! traceopts_closed,
    {stop, shutdown, State};

handle_event(#wx{event = #wxClose{type = close_window},
		userData = trace_options},
	     #traceopts_state{parent = Parent} = State) ->
    Parent ! traceopts_closed,
    {stop, shutdown, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Trace Options

handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}, userData = Boxes},
	     State) ->
    enable(Boxes),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Function options

handle_event(#wx{obj = ListBox,
		 event = #wxCommand{type = command_listbox_doubleclicked},
		 userData = trace_funcs},
	     #traceopts_state{frame = Frame, 
			      traced_funcs = TracedDict,
			      module_infobox_open = false} = State) ->
    ChosenModule = wxControlWithItems:getStringSelection(ListBox),
    CheckedFuncs = create_module_infobox(Frame, ChosenModule, TracedDict),
    {noreply, State#traceopts_state{module_infobox_open = true,
				    checked_funcs = CheckedFuncs}};

handle_event(#wx{obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = {From, Data, ListBox}},
	     #traceopts_state{checked_funcs = CheckedFuncs} = State) -> 
    
    Input = wxTextCtrl:getValue(TxtCtrl),
    FilteredData = [X || X <- Data, re:run(X, Input) =/= nomatch],
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, FilteredData),
    
    case From of
    	module_infobox ->
    	    lists:foreach(fun(Index) -> wxCheckListBox:check(ListBox, Index, [{check, true}]) end,
    			  [wxControlWithItems:findString(ListBox, X) || X <- CheckedFuncs, lists:member(X, FilteredData)]);
	trace_funcs ->
	    ignore
    end,
    {noreply, State};

handle_event(#wx{obj = Tree, event = #wxTree{type = command_tree_item_activated,
					     item = Item}},
	     #traceopts_state{frame = Frame, 
			      traced_funcs = TracedDict,
			      match_specs = MatchSpecs,
			      module_infobox_open = false} = State) ->
    
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Match specification"),
    {MatchPanel, MatchSz, ListBox} = create_matchspec_page(Dialog, MatchSpecs),
    ApplyBtn = wxButton:new(MatchPanel, ?wxID_OK, [{label, "Apply"}]),
    CancelBtn = wxButton:new(MatchPanel, ?wxID_CANCEL, []),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, ApplyBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    wxSizer:add(MatchSz, DialogBtnSz),

    TracedDict2 = case wxDialog:showModal(Dialog) of
		      ?wxID_OK ->
			  IntSelection = wxListBox:getSelection(ListBox),
			  StrSelection = wxControlWithItems:getString(ListBox, IntSelection),
			  MS = find_ms(StrSelection, MatchSpecs),
			  RootId = wxTreeCtrl:getRootItem(Tree),
			  ItemParent = wxTreeCtrl:getItemParent(Tree, Item),
			  
			  if (Item =:= RootId) ->
				  apply_matchspec(MS, TracedDict, root);
			     (ItemParent =:= RootId) ->
				  Module = list_to_atom(wxTreeCtrl:getItemText(Tree, Item)),
				  apply_matchspec(MS, TracedDict, {module, Module});
			     true -> 	
				  {Module, Function, Arity} = wxTreeCtrl:getItemData(Tree, Item),
				  apply_matchspec(MS, 
						  TracedDict, 
						  {function,
						   Module,
						   Function, 
						   Arity})
			  end;
		      ?wxID_CANCEL ->
			  TracedDict
		  end,
    {noreply, State#traceopts_state{traced_funcs = TracedDict2}};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Match specifications

handle_event(#wx{obj = ListBox,
		 event = #wxCommand{type = command_listbox_selected},
		 userData = {trace_matchspec, StyledTxtCtrl}},
	     #traceopts_state{match_specs = MatchSpecs} = State) ->
    SavedTxt = wxControlWithItems:getStringSelection(ListBox),
    MsOrFun = find_and_format_ms(SavedTxt, MatchSpecs),
    wxStyledTextCtrl:setText(StyledTxtCtrl, MsOrFun),
    {noreply, State};

handle_event(#wx{id = ?FUN2MS,
		 event = #wxCommand{type = command_button_clicked}},
	     #traceopts_state{frame = Frame} = State) ->
    %% FunTxt = wxStyledTextCtrl:getText(Fun2MSTxtCtrl),
    io:format("Not implemented... yet!~n"),
    %% {ok, Tokens,_} = erl_scan:string(FunTxt),
    %% io:format("Tokens ~p~n", [Tokens]),
    %% {ok, Term} = erl_parse:parse_term(Tokens),
    %% MatchSpec = dbg:fun2ms(Term),
    %% wxStyledTextCtrl:appendText(MSTxtCtrl, MatchSpec),
    {noreply, State};

handle_event(#wx{id = ?ADD_MS_BTN,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {StyledTxtCtrl, ListBox}},
	     #traceopts_state{frame = Frame,
			      match_specs = MatchSpecs} = State) ->
    StrMS = wxStyledTextCtrl:getText(StyledTxtCtrl),
    MatchSpecs2 = case check_correct_MS(StrMS) of
		      {true, TermMS} ->
			  wxControlWithItems:append(ListBox, StrMS),
			  lists:reverse([#match_spec{str_ms = StrMS, term_ms = TermMS} | MatchSpecs]);
		      false ->
			  MsgDialog = wxMessageDialog:new(Frame, "Invalid match specification"),
			  wxDialog:showModal(MsgDialog),
			  MatchSpecs
		  end,
    {noreply, State#traceopts_state{match_specs = MatchSpecs2}};


handle_event(#wx{id = ?ADD_MS_ALIAS_BTN,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Panel, StyledTxtCtrl, ListBox}},
	     #traceopts_state{frame = Frame, 
			      match_specs = MatchSpecs} = State) ->
    Dialog = wxTextEntryDialog:new(Panel, "Enter ms alias: "),
    MatchSpecs2 = case wxDialog:showModal(Dialog) of
		      ?wxID_OK ->
			  StrMS = wxStyledTextCtrl:getText(StyledTxtCtrl),
			  case check_correct_MS(StrMS) of
			      {true, TermMS} ->
				  Alias = wxTextEntryDialog:getValue(Dialog),
				  wxControlWithItems:append(ListBox, Alias),
				  lists:reverse([#match_spec{alias = Alias, str_ms = StrMS, 
							     term_ms = TermMS} | MatchSpecs]);
			      false ->
				  MsgDialog = wxMessageDialog:new(Frame, "Invalid match specification!"),
				  wxDialog:showModal(MsgDialog),
				  MatchSpecs
			  end;
		      ?wxID_CANCEL ->
			  MatchSpecs
		  end,
    {noreply, State#traceopts_state{match_specs = MatchSpecs2}};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Module info box

handle_event(#wx{id = ?wxID_OK, %Module_infobox OK
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_infobox, Dialog, Module, 
			     ParsedChoices, Choices}},
	     #traceopts_state{traced_funcs = TracedDict,
			      tree = Tree,
			      checked_funcs = CheckedFuncs} = State) ->
    
    Indices = [I+1 || I <- find_index(CheckedFuncs, ParsedChoices)],
    Selections = get_selections(Indices, Choices),
    TracedDict2 = case Selections of
		      [] ->
			  dict:erase(Module, TracedDict);
		      _ ->
			  Traced = [#traced_func{arity = Arity,
						 func_name = Function}
				    || {Function, Arity} <- Selections],
			  dict:store(Module, Traced, TracedDict)
		  end,
    
    update_tree(Tree, TracedDict2),
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#traceopts_state{traced_funcs = TracedDict2,
			  checked_funcs = [],
			  module_infobox_open = false}};

handle_event(#wx{id = ?wxID_CANCEL, %Module_infobox cancel
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_infobox, Dialog}},
	     State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#traceopts_state{module_infobox_open = false, checked_funcs = []}};

handle_event(#wx{id = ?SELECT, %Module_infobox select/deselect
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Bool, CheckListBox}},
	     #traceopts_state{checked_funcs = CheckedFuncs} = State) ->
    {_, Selections} = wxListBox:getSelections(CheckListBox),
    lists:foreach(fun(Index) -> wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]) end, Selections),
    StrSelections = [wxControlWithItems:getString(CheckListBox, N) || N <- Selections],
    CheckedFuncs2 = case Bool of
			true ->
			    [X || X <- StrSelections,
				  not(lists:member(X, CheckedFuncs))] ++ CheckedFuncs;
			false ->
			    CheckedFuncs -- StrSelections
		    end,
    {noreply, State#traceopts_state{checked_funcs = CheckedFuncs2}};

handle_event(#wx{id = ?SELECT_ALL, %Module_infobox select-/deselect-all
		 event = #wxCommand{type = command_button_clicked},
		 userData = {Bool, CheckListBox}},
	     State) ->
    lists:foreach(fun(Index) -> 
			  wxCheckListBox:check(CheckListBox, Index, [{check, Bool}])
		  end,
		  lists:seq(0, wxControlWithItems:getCount(CheckListBox))),
    CheckedFuncs = case Bool of
		       true ->
			   [wxControlWithItems:getString(CheckListBox, N) 
			    || N <- lists:seq(0, wxControlWithItems:getCount(CheckListBox))];
		       false ->
			   []
		   end,
    {noreply, State#traceopts_state{checked_funcs = CheckedFuncs}};

handle_event(#wx{obj = CheckListBox,
		 event = #wxCommand{type = command_checklistbox_toggled,
				    commandInt = Index}},
	     #traceopts_state{checked_funcs = CheckedFuncs} = State) ->

    UpdCheckedFuncs = case 
			  wxCheckListBox:isChecked(CheckListBox, Index) of
			  true ->
			      [wxControlWithItems:getString(CheckListBox, Index) | CheckedFuncs];
			  false ->
			      lists:delete(wxControlWithItems:getString(CheckListBox, Index), CheckedFuncs) 
		      end,
    {noreply, State#traceopts_state{checked_funcs = UpdCheckedFuncs}};

handle_event(#wx{event = #wxClose{type = close_window},
		 userData = {module_infobox, Dialog}}, State) ->
    wxDialog:show(Dialog, [{show, false}]),
    {noreply, State#traceopts_state{module_infobox_open = false, 
				    checked_funcs = []}};

handle_event(#wx{event = What}, State) ->
    io:format("~p~p: Unhandled event: ~p ~n", [?MODULE, self(), What]),
    {noreply, State}.



terminate(Reason, #traceopts_state{frame = Frame}) ->
    io:format("~p terminating traceopts. Reason: ~p~n", [?MODULE, Reason]),
    wxFrame:destroy(Frame),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.
