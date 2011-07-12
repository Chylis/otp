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
	  boxes,
	  functionpage_listbox,
	  matchpage_styled_txtctrl,
	  matchpage_listbox,
	  popup_open = false,
	  module_popup_dialog,
	  module_popup_checklistbox,
	  matchspec_popup_dialog,
	  matchspec_popup_listbox,
	  matchspec_popup_styled_txtctrl,
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

-define(TRACEOPTS_FRAME, 9).

-define(MATCHPAGE_FUN2MS, 13).
-define(MATCHPAGE_ADDMS, 14).
-define(MATCHPAGE_ADDMS_ALIAS, 15).
-define(MATCHPAGE_LISTBOX, 17).

-define(MODULEPOPUP_SELECT, 20).
-define(MODULEPOPUP_SELALL, 21).
-define(MODULEPOPUP_CHECKLISTBOX, 22).
-define(MODULEPOPUP_TXTCTRL, 23).
-define(MODULEPOPUP_DIALOG, 24).

-define(FUNCTIONPAGE_LISTBOX, 25).
-define(FUNCTIONPAGE_TXTCTRL, 26).


start(ParentFrame, ParentPid, Node, TraceOpts, TracedFuncs, MatchSpecs) ->
    wx_object:start(?MODULE, [ParentFrame, ParentPid, Node, TraceOpts, 
			      TracedFuncs, MatchSpecs], []).

init([ParentFrame, ParentPid, Node, TraceOpts, TracedFuncs, MatchSpecs]) ->
    {Frame, Tree, Boxes, ModuleListBox, MatchTxtCtrl, MatchListBox} = setup(ParentFrame, Node, TraceOpts, TracedFuncs, MatchSpecs),
    {Frame, #traceopts_state{
       parent = ParentPid,
       frame = Frame,
       tree = Tree,
       functionpage_listbox = ModuleListBox,
       matchpage_styled_txtctrl = MatchTxtCtrl,
       matchpage_listbox = MatchListBox,
       boxes = Boxes,
       match_specs = MatchSpecs,
       traced_funcs = TracedFuncs,
       trace_options = TraceOpts}}.

setup(ParentFrame, Node, TraceOpts, TracedFuncs, MatchSpecs) ->

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup main window
    
    Frame = wxFrame:new(ParentFrame, ?TRACEOPTS_FRAME, "Trace options",
			[{size, {400, 500}}]),
    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Notebook = wxNotebook:new(Panel, ?wxID_ANY),
    Modules = get_modules(Node),
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Setup tracing page
    
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
    
%%%%%%%%%%%%%%%%%%%%%%%% Setup functions page

    FuncPanel = wxPanel:new(Notebook),
    FuncMainSz = wxBoxSizer:new(?wxVERTICAL),
    ModuleSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Select module"}]),
    TreeSz = wxStaticBoxSizer:new(?wxVERTICAL, FuncPanel, [{label, "Selected functions"}]),
    
    AllModules = atomlist_to_stringlist(Modules),
    ModuleTxtCtrl = wxTextCtrl:new(FuncPanel, ?FUNCTIONPAGE_TXTCTRL),
    ModuleListBox = wxListBox:new(FuncPanel, ?FUNCTIONPAGE_LISTBOX, [{choices, AllModules}, {style, ?wxLB_SINGLE}]),
    TreeCtrl = wxTreeCtrl:new(FuncPanel),
    wxTreeCtrl:addRoot(TreeCtrl, atom_to_list(Node)),
    update_tree(TreeCtrl, TracedFuncs), 
    
    wxTextCtrl:connect(ModuleTxtCtrl, command_text_updated, 
		       [{userData, AllModules}]),
    wxListBox:connect(ModuleListBox, command_listbox_doubleclicked), 
    wxTreeCtrl:connect(TreeCtrl, command_tree_item_activated),

    wxSizer:add(ModuleSz, ModuleTxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(ModuleSz, ModuleListBox, [{flag, ?wxEXPAND}]),
    wxSizer:add(TreeSz, TreeCtrl, [{flag, ?wxEXPAND},{proportion, 1}]),
    wxSizer:add(FuncMainSz, ModuleSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(FuncMainSz, TreeSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxWindow:setSizer(FuncPanel, FuncMainSz),
    wxNotebook:addPage(Notebook, FuncPanel, "Functions"),

    
%%%%%%%%%%%%%%%%%%% Setup match specification page
    
    {MatchPanel, _, MatchTxtCtrl, MatchListBox} = create_matchspec_page(Notebook, MatchSpecs, matchpage),
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

    wxButton:connect(OKBtn, command_button_clicked, [{userData, trace_options}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, trace_options}]),
    wxFrame:connect(Frame, close_window, []),
    wxFrame:show(Frame),
    {Frame, TreeCtrl, Boxes, ModuleListBox, MatchTxtCtrl, MatchListBox}.


filter_listbox_data(Input, Data, ListBox) ->
    FilteredData = [X || X <- Data, re:run(X, Input) =/= nomatch],
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, FilteredData),
    FilteredData.

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
    #match_spec{};
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
    {ok, dict:map(UpdateMS, TracedDict)};
apply_matchspec(MatchSpec, TracedDict, {module, Module}) ->
    RecordList = dict:fetch(Module, TracedDict),
    RecordList2 = [X#traced_func{match_spec = MatchSpec} || X <- RecordList],
    {ok, dict:store(Module, RecordList2, TracedDict)};
apply_matchspec(MatchSpec, TracedDict, {function, Module, TracedFuncRec}) -> %%Module, Function, Arity}) ->
    RecordList = dict:fetch(Module, TracedDict),
    NewFunc = TracedFuncRec#traced_func{match_spec = MatchSpec},
    RecordList2 = [NewFunc | [X || X <- RecordList, X =/= TracedFuncRec]],
    {NewFunc, dict:store(Module, RecordList2, TracedDict)}.

create_matchspec_page(Parent, MatchSpecs, UserData) ->
    Panel = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TxtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Match specification:"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    SavedSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Saved match specifications:"}]),

    TxtCtrl = create_styled_txtctrl(Panel),
    wxSizer:add(TxtSz, TxtCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    AddMsBtn = wxButton:new(Panel, ?MATCHPAGE_ADDMS, [{label, "Add"}]),
    AddMsAliasBtn = wxButton:new(Panel, ?MATCHPAGE_ADDMS_ALIAS, [{label, "Add with alias"}]),
    Fun2MSBtn = wxButton:new(Panel, ?MATCHPAGE_FUN2MS, [{label, "Fun2ms"}]),
    wxSizer:add(BtnSz, AddMsBtn),
    wxSizer:add(BtnSz, AddMsAliasBtn),
    wxSizer:add(BtnSz, Fun2MSBtn),

    Choices = show_ms_in_savedlistbox(MatchSpecs),
    SavedMSListBox = wxListBox:new(Panel, ?MATCHPAGE_LISTBOX, [{choices, Choices}]),
    wxSizer:add(SavedSz, SavedMSListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxButton:connect(AddMsBtn, command_button_clicked, [{userData, UserData}]),
    wxButton:connect(AddMsAliasBtn, command_button_clicked, [{userData, UserData}] ),
    wxButton:connect(Fun2MSBtn, command_button_clicked, [{userData, UserData}] ),
    wxListBox:connect(SavedMSListBox, command_listbox_selected, [{userData, UserData}] ),
    wxSizer:add(MainSz, TxtSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSz, BtnSz),
    wxSizer:add(MainSz, SavedSz, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxWindow:setSizer(Panel, MainSz),
    {Panel, MainSz, TxtCtrl, SavedMSListBox}.



update_tree(Tree, Dict) ->
    RootId = wxTreeCtrl:getRootItem(Tree),
    wxTreeCtrl:deleteChildren(Tree, RootId),
    
    FillTree = fun(KeyAtom, RecordList, acc_in) ->
		       ParsedList = parse_record_function_names(RecordList),
		       Module = wxTreeCtrl:appendItem(Tree, RootId, atom_to_list(KeyAtom)),
		       lists:foldl(fun(TracedFuncRecord, N) ->
					   FNameStr = lists:nth(N, ParsedList),
					   wxTreeCtrl:appendItem(Tree, Module, FNameStr,
								 [{data, TracedFuncRecord}]),
					   N+1
				   end,
				   1, RecordList),
		       wxTreeCtrl:sortChildren(Tree, Module),
		       acc_in
	       end,
    dict:fold(FillTree, acc_in, Dict),
    wxTreeCtrl:sortChildren(Tree, RootId),
    wxTreeCtrl:expand(Tree, RootId).




create_module_popup(Parent, ModuleName, TracedDict) ->
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

    Dialog = wxDialog:new(Parent, ?MODULEPOPUP_DIALOG, ModuleName),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    SelBtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    TxtCtrl = wxTextCtrl:new(Panel, ?MODULEPOPUP_TXTCTRL),
    SelBtn = wxButton:new(Panel, ?MODULEPOPUP_SELECT, [{label, "Select"}]),
    DeSelBtn = wxButton:new(Panel, ?MODULEPOPUP_SELECT, [{label, "Deselect"}]),
    SelAllBtn = wxButton:new(Panel, ?MODULEPOPUP_SELALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?MODULEPOPUP_SELALL, [{label, "Deselect all"}]),
    CheckListBox = wxCheckListBox:new(Panel, ?MODULEPOPUP_CHECKLISTBOX, [{choices, ParsedChoices}, {style, ?wxLB_EXTENDED}]),
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

    wxButton:connect(SelBtn, command_button_clicked, [{userData, true}]),
    wxButton:connect(DeSelBtn, command_button_clicked, [{userData, false}]),
    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, true}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, false}]),
    wxButton:connect(OKBtn, command_button_clicked, [{userData, {module_popup, Module, ParsedChoices, Choices}}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, module_popup}]),
    wxTextCtrl:connect(TxtCtrl, command_text_updated, [{userData, ParsedChoices}]),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
    wxDialog:connect(Dialog, close_window),
    wxDialog:show(Dialog),
    {Dialog, CheckListBox, Selections}.

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


check_correct_MS([]) ->
    false;
check_correct_MS(String) ->
    Tokens =
	case lists:last(String) =:= $. of
	    true ->
		try_scan(String);
	    false ->
		try_scan(String ++ ".")
	end,
    io:format("Tokens: ~p~n", [Tokens]),
    
    case try_parse(Tokens) of
	{ok, Term} ->
	    check_correct_ms_format(Term);
	error ->
	    false
    end.
    

try_scan(String) ->
    try	
	erl_scan:string(String) of
	{ok, T, _} ->
	    T;
	_ ->
	    error
    catch 
	_:_ -> error
    end.

try_parse(Tokens) ->
    try
	erl_parse:parse_term(Tokens) of
	{ok, Term} ->
	    {ok, Term};
	_ ->
	    error
    catch
	_:_ ->
	    error
    end.
	

check_correct_ms_format([H|T] = Term) when length(Term) >= 1,
					   is_tuple(H), size(H) =:= 3->
    N = length(T),
    check_correct_ms_format(T, N, Term);
check_correct_ms_format(_Other) ->
    false.

check_correct_ms_format([], 0, Term) ->
    {true, Term};
check_correct_ms_format([H|T], N, Term) when is_tuple(H), size(H) =:= 3 ->
    check_correct_ms_format(T, N-1, Term);
check_correct_ms_format(_Other, _, _) ->
    false.
    


    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Trace option window

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% All pages

handle_event(#wx{id = ?wxID_OK,
		 event = #wxCommand{type = command_button_clicked},
		userData = trace_options}, 
	     #traceopts_state{boxes = Boxes,
			      trace_options = TraceOpts, 
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

handle_event(#wx{id = ?TRACEOPTS_FRAME,
		 event = #wxClose{type = close_window}},
		 #traceopts_state{parent = Parent} = State) ->
    Parent ! traceopts_closed,
    {stop, shutdown, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Tracing

handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}, userData = Boxgroup},
	     State) ->
    enable(Boxgroup),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Functions

handle_event(#wx{id = ?FUNCTIONPAGE_LISTBOX,
		 obj = ListBox,
		 event = #wxCommand{type = command_listbox_doubleclicked}},
	     #traceopts_state{frame = Frame, 
			      traced_funcs = TracedDict,
			      popup_open = false} = State) ->
    ChosenModule = wxControlWithItems:getStringSelection(ListBox),
    {Dialog, CheckListBox, CheckedFuncs} = create_module_popup(Frame, ChosenModule, TracedDict),
    {noreply, State#traceopts_state{popup_open = true,
				    module_popup_dialog = Dialog,
				    module_popup_checklistbox = CheckListBox,
				    checked_funcs = CheckedFuncs}};

handle_event(#wx{id = ?FUNCTIONPAGE_TXTCTRL,
		 obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = Data},
	     #traceopts_state{functionpage_listbox = ListBox} = State) ->
			      %% checked_funcs = CheckedFuncs} = State
    Input = wxTextCtrl:getValue(TxtCtrl),
    filter_listbox_data(Input, Data, ListBox),
    {noreply, State};

handle_event(#wx{event = #wxTree{type = command_tree_item_activated,
				 item = Item}},
	     #traceopts_state{frame = Frame, 
			      match_specs = MatchSpecs,
			      popup_open = false} = State) ->
    
    Dialog = wxDialog:new(Frame, ?wxID_ANY, "Match specification"),
    {MatchPanel, MatchSz, StyledTxtCtrl, ListBox} = create_matchspec_page(Dialog, MatchSpecs, matchpopup),
    ApplyBtn = wxButton:new(MatchPanel, ?wxID_APPLY),
    CancelBtn = wxButton:new(MatchPanel, ?wxID_CANCEL, []),
    wxButton:connect(ApplyBtn, command_button_clicked, [{userData, Item}]),
    wxButton:connect(CancelBtn, command_button_clicked, [{userData, matchspec_popup}]),
    DialogBtnSz = wxStdDialogButtonSizer:new(),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, ApplyBtn),
    wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
    wxStdDialogButtonSizer:realize(DialogBtnSz),
    wxSizer:add(MatchSz, DialogBtnSz),
    
    wxDialog:show(Dialog, [{show, true}]),
    {noreply, State#traceopts_state{matchspec_popup_dialog = Dialog,
				    matchspec_popup_listbox = ListBox,
				    matchspec_popup_styled_txtctrl = StyledTxtCtrl,
				    popup_open = true}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Match specs

handle_event(#wx{id = ?MATCHPAGE_LISTBOX,
		 obj = ListBox,
		 event = #wxCommand{type = command_listbox_selected},
		 userData = From},
	     #traceopts_state{match_specs = MatchSpecs} = State) ->
    io:format("Selected ~n"),
    
    StyledTxtCtrl = case From of
    			matchpage ->
    			    State#traceopts_state.matchpage_styled_txtctrl;
    			matchpopup ->
    			    State#traceopts_state.matchspec_popup_styled_txtctrl
    		    end,
    SavedTxt = wxControlWithItems:getStringSelection(ListBox),
    MsOrFun = find_and_format_ms(SavedTxt, MatchSpecs),
    wxStyledTextCtrl:setText(StyledTxtCtrl, MsOrFun),
    {noreply, State};

handle_event(#wx{id = ?MATCHPAGE_FUN2MS,
		 event = #wxCommand{type = command_button_clicked},
		 userData = From},
	     #traceopts_state{frame = Frame} = State) ->
    io:format("Not implemented... yet!~n"),
    {noreply, State};

handle_event(#wx{id = ?MATCHPAGE_ADDMS,
		 event = #wxCommand{type = command_button_clicked},
		 userData = From},
	     #traceopts_state{match_specs = MatchSpecs} = State) ->
    
    {StyledTxtCtrl, ListBox, Frame} = case From of
					  matchpage ->
					      {State#traceopts_state.matchpage_styled_txtctrl,
					       State#traceopts_state.matchpage_listbox,
					       State#traceopts_state.frame};
					  matchpopup ->
					      {State#traceopts_state.matchspec_popup_styled_txtctrl,
					       State#traceopts_state.matchspec_popup_listbox,
					State#traceopts_state.matchspec_popup_dialog}
				      end,
    
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


handle_event(#wx{id = ?MATCHPAGE_ADDMS_ALIAS,
		 event = #wxCommand{type = command_button_clicked},
		 userData = From},
	     #traceopts_state{match_specs = MatchSpecs} = State) ->
        
    {StyledTxtCtrl, ListBox, Frame} = case From of
					  matchpage ->
					      {State#traceopts_state.matchpage_styled_txtctrl,
					       State#traceopts_state.matchpage_listbox,
					       State#traceopts_state.frame};
					  matchpopup ->
					      {State#traceopts_state.matchspec_popup_styled_txtctrl,
					       State#traceopts_state.matchspec_popup_listbox,
					       State#traceopts_state.matchspec_popup_dialog}
				      end,
    
    StrMS = wxStyledTextCtrl:getText(StyledTxtCtrl),
    MatchSpecs2 = case check_correct_MS(StrMS) of
		      {true, TermMS} ->
			  Dialog = wxTextEntryDialog:new(Frame, "Enter ms alias: "),
			  Alias = case wxDialog:showModal(Dialog) of
				      ?wxID_OK ->
					  wxTextEntryDialog:getValue(Dialog);
				      ?wxID_CANCEL ->
					  ""
				  end,
			  case Alias of
			      "" ->
				  MatchSpecs;
			      _ ->
				  wxControlWithItems:append(ListBox, Alias),
				  lists:reverse([#match_spec{alias = Alias, str_ms = StrMS, 
							     term_ms = TermMS} | MatchSpecs])
			  end;
		      false ->
			  MsgDialog = wxMessageDialog:new(Frame, "Invalid match specification!"),
			  wxDialog:showModal(MsgDialog),
			  MatchSpecs
		  end,
    {noreply, State#traceopts_state{match_specs = MatchSpecs2}};

handle_event(#wx{id = ?wxID_APPLY,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Item},
	     #traceopts_state{matchspec_popup_dialog = Dialog,
			      matchspec_popup_listbox = ListBox,
			      tree = Tree,
			      match_specs = MatchSpecs,
			      traced_funcs = TracedDict} = State) ->

    IntSelection = wxListBox:getSelection(ListBox),
    StrSelection = wxControlWithItems:getString(ListBox, IntSelection),
    MS = find_ms(StrSelection, MatchSpecs),
    RootId = wxTreeCtrl:getRootItem(Tree),
    ItemParent = wxTreeCtrl:getItemParent(Tree, Item),
    
    TracedDict2 = 
	if (Item =:= RootId) ->
		{ok, NewDict} = apply_matchspec(MS, TracedDict, root),
		NewDict;
	   (ItemParent =:= RootId) ->
		Module = list_to_atom(wxTreeCtrl:getItemText(Tree, Item)),
		{ok, NewDict} = apply_matchspec(MS, TracedDict, {module, Module}),
		NewDict;
	   true -> 
		TracedFuncRec = wxTreeCtrl:getItemData(Tree, Item),
		Module = list_to_atom(wxTreeCtrl:getItemText(Tree, ItemParent)),
		{NewTracedFuncRecord, NewDict} = 
		    apply_matchspec(MS, 
				    TracedDict, 
				    {function,
				     Module,
				     TracedFuncRec}),
		wxTreeCtrl:setItemData(Tree, Item, NewTracedFuncRecord),
		NewDict
	end,

    wxDialog:destroy(Dialog),
    {noreply, State#traceopts_state{traced_funcs = TracedDict2,
				    popup_open = false}};

handle_event(#wx{id = ?wxID_CANCEL,
		 event = #wxCommand{type = command_button_clicked},
		userData = matchspec_popup},
	     #traceopts_state{matchspec_popup_dialog = Dialog} = State) ->
    wxDialog:destroy(Dialog),
    {noreply, State#traceopts_state{popup_open = false}};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Module Popup

handle_event(#wx{id = ?wxID_OK,
		 event = #wxCommand{type = command_button_clicked},
		 userData = {module_popup, Module, 
			     ParsedChoices, Choices}},
	     #traceopts_state{
		  module_popup_dialog = Dialog,
		  traced_funcs = TracedDict,
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
    wxDialog:destroy(Dialog),
    {noreply, State#traceopts_state{traced_funcs = TracedDict2,
			  checked_funcs = [],
			  popup_open = false}};

handle_event(#wx{id = ?wxID_CANCEL,
		 event = #wxCommand{type = command_button_clicked},
		userData = module_popup},
	     #traceopts_state{module_popup_dialog = Dialog} = State) ->
    wxDialog:destroy(Dialog),
    {noreply, State#traceopts_state{popup_open = false, 
				    checked_funcs = []}};

handle_event(#wx{id = ?MODULEPOPUP_SELECT,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Bool},
	     #traceopts_state{module_popup_checklistbox = CheckListBox,
			      checked_funcs = CheckedFuncs} = State) ->
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

handle_event(#wx{id = ?MODULEPOPUP_SELALL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Bool},
	     #traceopts_state{module_popup_checklistbox = CheckListBox} = State) ->
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

handle_event(#wx{id = ?MODULEPOPUP_CHECKLISTBOX,
		 obj = CheckListBox,
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

handle_event(#wx{id = ?MODULEPOPUP_TXTCTRL,
		 obj = TxtCtrl,
		 event = #wxCommand{type = command_text_updated},
		 userData = Data},
	     #traceopts_state{module_popup_checklistbox = CListBox,
			      checked_funcs = CheckedFuncs} = State) ->
    Input = wxTextCtrl:getValue(TxtCtrl),
    FilteredData = filter_listbox_data(Input, Data, CListBox),
    lists:foreach(fun(Index) -> 
			  wxCheckListBox:check(CListBox, Index, [{check, true}]) 
		  end,
		  [wxControlWithItems:findString(CListBox, X) || X <- CheckedFuncs, lists:member(X, FilteredData)]),
    {noreply, State};

handle_event(#wx{id = ?MODULEPOPUP_DIALOG,
		 event = #wxClose{type = close_window}},
	     #traceopts_state{module_popup_dialog = Dialog} = State) ->
    wxDialog:destroy(Dialog),
    {noreply, State#traceopts_state{popup_open = false, 
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
