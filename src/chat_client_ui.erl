%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, Telecom Lille
%%% @doc
%%%
%%% @end
%%% Created : 29. oct. 2015 09:27
%%%-------------------------------------------------------------------
-module(chat_client_ui).

%% API
-include_lib("wx/include/wx.hrl").

-export([start/0, notify_data/2,notify_absence/1,notify_presence/1]).

start()->
  register(ui_pid, spawn(fun()->
    Wx = wx:new(),
    State = wx:batch(fun() -> create_window(Wx) end),
    loop(State)
                         end)),
  ok.

create_window(Wx)->

  %% Create Frame

  Parent = wxFrame:new(Wx,
    -1,
    "Chat client",
    [{size, {500, 400}}]),

  %% Create panel
  Panel = wxScrolledWindow:new(Parent, []),
  %% Create menu bar
  Menubar = wxMenuBar:new(),
  %% Create menu
  Menuconnexion = wxMenu:new(),
  wxMenu:append(Menuconnexion,?wxID_OPEN, "&Connect"),
  %% create and add the status bar
  wxFrame:createStatusBar(Parent),

  %% Create the chat text widget
  Outputtextwg = wxTextCtrl:new(Panel,1, [{value, ""}, {size, {300, 250}},{style,?wxTE_MULTILINE}]),
  wxTextCtrl:setEditable(Outputtextwg,true),

  %% Create the connected users list widget
  ConnectedUsers = [],
  Listboxwg = wxListBox:new(Panel, 2, [{size, {150,220}},
    {choices, [""| ConnectedUsers]},
    {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(Listboxwg, "List of connected users"),

  %% Create the chat text input widget
  Sendtextwg = wxTextCtrl:new(Panel,3, [{value, "Enter text here"}, {size, {300, 20}}]),

  %% Create the send button widget
  Sendbutton = wxButton:new(Panel, 4, [{label, "&Send"}]),

  %% Create the exit button widget
  Exitbutton = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),


  %% Setup sizers
  OuterSizer = wxBoxSizer:new(?wxHORIZONTAL),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  ChatSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ListBoxSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Connected users"}]),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  %% Add sizers and widgets
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(ChatSizer, Outputtextwg, []),
  wxSizer:addSpacer(ChatSizer, 10),
  wxSizer:add(ListBoxSizer, Listboxwg,[]),
  wxSizer:add(ChatSizer,ListBoxSizer,[]),
  wxSizer:add(MainSizer,ChatSizer,[]),
  wxSizer:addSpacer(MainSizer, 10),

  wxSizer:add(MainSizer, Sendtextwg, []),

  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(ButtonSizer, Sendbutton, []),

  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(ButtonSizer, Exitbutton, []),
  wxSizer:add(MainSizer, ButtonSizer, []),

  wxSizer:addSpacer(OuterSizer, 10),
  wxSizer:add(OuterSizer, MainSizer, []),

  %% Set every element
  wxPanel:setSizer(Panel, OuterSizer),
  wxFrame:setMenuBar(Parent, Menubar),
  wxFrame:setStatusText(Parent,"Status: Started."),
  wxScrolledWindow:setScrollRate(Panel, 5, 5),

  %% Add menu bar
  wxMenuBar:append(Menubar, Menuconnexion, "Connexion"),

  %% Set events
  wxFrame:connect(Parent, close_window),
  wxFrame:connect(Parent, command_button_clicked),
  wxFrame:connect(Sendtextwg, set_focus),
  wxFrame:connect(Parent, command_menu_selected),

  wxFrame:show(Parent),

  {Parent, Outputtextwg, Sendtextwg, Listboxwg}.


loop(State) ->
  {Frame, Outputtextwg, Sendtextwg, Listboxwg} = State,

  receive
    {message, Message, Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      Sendtextval = Name ++ ": " ++ Message ++ "\n",
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Sendtextval),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
    {presence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Name ++ " connected.\n"),
      wxListBox:append(Listboxwg,Name),
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
    {absence,Name} ->
      wxFrame:setStatusText(Frame,"Status: Receiving status..."),
      IndexOfName = wxListBox:findString(Listboxwg,Name),
      if IndexOfName > -1 ->
        wxListBox:delete(Listboxwg,IndexOfName),
        wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ Name ++ " disconnected.\n");
        true -> error_logger:error_msg("Unknown client name tried to disconnect from UI")
      end,
      wxFrame:setStatusText(Frame,"Status: Finished."),
      loop(State);
  %% the Send button is clicked
    #wx{id = 4, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Sending..."),
      Sendtextval = wxTextCtrl:getValue(Sendtextwg),
      ToNameindex = wxListBox:getSelection(Listboxwg),
      if ToNameindex =< 0 ->
        chat_client:send(Sendtextval);
        ToNameindex > 0 ->
          chat_client:send(Sendtextval,wxListBox:getStringSelection(Listboxwg));
        true ->
          chat_client:send(Sendtextval)
      end,
      wxTextCtrl:setValue(Outputtextwg, wxTextCtrl:getValue(Outputtextwg) ++ "You: " ++ Sendtextval ++ "\n"),
      wxFrame:setStatusText(Frame,"Status: Message sent."),
      wxTextCtrl:clear(Sendtextwg),
      wxTextCtrl:setFocus(Sendtextwg),
      loop(State);
  %% the send text control is focused
    #wx{id = 3, event = #wxFocus{type = set_focus}} ->
      Sendtextval = wxTextCtrl:getValue(Sendtextwg),
      if Sendtextval =:= "Enter text here" ->
        wxTextCtrl:clear(Sendtextwg);
        true -> wxTextCtrl:setFocus(Sendtextwg)
      end,
      loop(State);
  %% The connect menu item is clicked
    #wx{id = ?wxID_OPEN, event = #wxCommand{type = command_menu_selected}} ->
      chat_client:start("127.0.0.1",13),
      loop(State);
  %% The exit button is clicked
    #wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}} ->
      wxFrame:setStatusText(Frame,"Status: Quitting..."),
      wxWindow:destroy(Frame),
      ok;
  %% Window Close Event
    #wx{event=#wxClose{}} ->
      io:format("~p Closing window ~n",[self()]),
      chat_client:disconnect(),
      wxWindow:destroy(Frame),
      ok;
  %% default handling
    _ ->
      loop(State)
  end.



notify_data(Message, Name) ->
  ui_pid ! {message,Message, Name},
  ok.

notify_presence(Name) ->
  ui_pid ! {presence,Name},
  ok.

notify_absence(Name) ->
  ui_pid ! {absence, Name},
  ok.