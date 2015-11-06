%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Telecom Lille
%%% @doc
%%%
%%% @end
%%% Created : 29. oct. 2015 09:27
%%%-------------------------------------------------------------------
-module(chat_ui).

-compile(export_all).
-include_lib("wx/include/wx.hrl").



start() ->
  State = make_window(),
  loop(State).



make_window() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "Chat client", [{size, {475, 450}}]),
  Panel = wxPanel:new(Frame),

  %% creating menu bar
  Menubar = wxMenuBar:new(),

  %% creating menu
  Menuconnexion = wxMenu:new(),
  wxMenu:append(Menuconnexion,?wxID_OPEN, "&Connect to..."),

  %% adding menu bar
  wxMenuBar:append(Menubar, Menuconnexion, "Connexion"),


  %% creating status bar
  wxFrame:createStatusBar(Frame),

  %% create widgets
  Outputtextwg = wxTextCtrl:new(Panel, 1001, [{value, ""}, {size, {300, 300}}, {style, ?wxTE_MULTILINE}]),
  Sendtextwg = wxTextCtrl:new(Panel, 2001, [{value, "Enter text here"}, {size, {300, 20}}]), %set default value
  Sendbutton = wxButton:new(Panel, 101, [{label, "&Send"}]),
  Exitbutton = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),

  %% create sizers
  OuterSizer = wxBoxSizer:new(?wxHORIZONTAL),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  %ConnectedSizer  = wxStaticBoxSizer:new(?wxHORIZONTAL, ,[{label, "Connected people:"}]),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  %% adding sizers and widgets
  wxSizer:addSpacer(MainSizer, 5),  %spacer
  wxSizer:add(MainSizer, Outputtextwg, []),
  wxSizer:addSpacer(MainSizer, 10),  %spacer

  wxSizer:add(MainSizer, Sendtextwg, []),

  wxSizer:addSpacer(MainSizer, 5),  %spacer
  wxSizer:add(ButtonSizer, Sendbutton, []),

  wxSizer:addSpacer(ButtonSizer, 5),  %spacer
  wxSizer:add(ButtonSizer, Exitbutton, []),
  wxSizer:add(MainSizer, ButtonSizer, []),

  wxSizer:addSpacer(OuterSizer, 20), % spacer
  wxSizer:add(OuterSizer, MainSizer, []),

  %% set every element
  wxPanel:setSizer(Panel, OuterSizer),
  wxFrame:setMenuBar(Frame, Menubar),
  wxFrame:setStatusText(Frame,"Status: Started."),
  wxFrame:show(Frame),

  %% create four listeners
  wxFrame:connect(Frame, close_window),
  wxPanel:connect(Panel, command_button_clicked),
  wxPanel:connect(Panel, command_menu_selected),
  wxTextCtrl:connect(Panel, set_focus),

  {Frame, Outputtextwg, Sendtextwg}.



loop(State) ->
  {Frame, Outputtextwg, Sendtextwg} = State,

  receive

    %% a connection get the close_window signal and sends this message to the server
    #wx{event = #wxClose{}} ->
    wxFrame:setStatusText(Frame,"Status: Quitting..."),
    wxWindow:destroy(Frame),
    ok;

    %%  the exit button is clicked
    #wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      wxWindow:destroy(Frame),
      ok;

    %% the Send button (ID 101) is clicked
    #wx{id = 101, event = #wxCommand{type = command_button_clicked}} ->
      %%TODO remplacer par appel au serveur
      wxFrame:setStatusText(Frame,"Status: Sending..."),
      Sendtextval = "You: " ++ wxTextCtrl:getValue(Sendtextwg) ++ "\n",
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Sendtextval),
      wxFrame:setStatusText(Frame,"Status: Sent."),
      wxTextCtrl:clear(Sendtextwg),
      wxTextCtrl:setFocus(Sendtextwg),
      loop(State);

    %% the 'Connect to menu' is clicked
    #wx{id = ?wxID_OPEN, event = #wxCommand{type = command_menu_selected}} ->
      MD = wxMessageDialog:new(Frame,"A faire",[{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "Se connecter ?..."}]),
      wxDialog:showModal(MD),
      wxDialog:destroy(MD),
      loop(State);

    %% default handling
    Msg ->
      io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
      loop(State)

  end.