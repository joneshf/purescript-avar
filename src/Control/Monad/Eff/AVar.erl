-module(control_monad_eff_aVar@foreign).
-behavior(gen_statem).

%% PS FFI functions
-export(['_killVar'/0, '_putVar'/0, '_readVar'/0, '_status'/0, '_takeVar'/0]).
-export(['_tryPutVar'/0, '_tryReadVar'/0, '_tryTakeVar'/0]).
-export([makeEmptyVar/0, makeVar/1]).

-export([callback_mode/0, init/1]).
-export([handle_event/4]).

'_killVar'() ->
    fun(Util, Error, AVar) ->
        fun() ->
            gen_statem:call(AVar, {kill, Util, Error})
        end
    end.

'_putVar'() ->
    fun(Util, Value, AVar, CB) ->
        fun() ->
            {UniqCB, Canceller} = unique_canceller(AVar, {Value, CB}),
            gen_statem:cast(AVar, {put, Util, UniqCB}),
            Canceller
        end
    end.

'_readVar'() ->
    fun(Util, AVar, CB) ->
        fun() ->
            {UniqCB, Canceller} = unique_canceller(AVar, CB),
            gen_statem:cast(AVar, {read, Util, UniqCB}),
            Canceller
        end
    end.

'_status'() ->
    fun(Util, AVar) ->
        fun() ->
            gen_statem:call(AVar, {status, Util})
        end
    end.

'_takeVar'() ->
    fun(Util, AVar, CB) ->
        fun() ->
            {UniqCB, Canceller} = unique_canceller(AVar, CB),
            gen_statem:cast(AVar, {take, Util, UniqCB}),
            Canceller
        end
    end.

'_tryPutVar'() ->
    fun(Util, Value, AVar) ->
        fun() ->
            gen_statem:call(AVar, {tryPut, Util, Value})
        end
    end.

'_tryReadVar'() ->
    fun(Util, AVar) ->
        fun() ->
            gen_statem:call(AVar, {tryRead, Util})
        end
    end.

'_tryTakeVar'() ->
    fun(Util, AVar) ->
        fun() ->
            gen_statem:call(AVar, {tryTake, Util})
        end
    end.

makeEmptyVar() ->
    fun() ->
        {ok, AVar} = gen_statem:start(?MODULE, empty, []),
        AVar
    end.

makeVar(Value) ->
    fun() ->
        {ok, AVar} = gen_statem:start(?MODULE, {filled, Value}, []),
        AVar
    end.

%% gen_statem mandatory callbacks

callback_mode() -> handle_event_function.

init(empty) -> {ok, empty, empty_queues()};
init({filled, Value}) -> {ok, {filled, Value}, filled_queues()}.

%% gen_statem callbacks

handle_event({call, From},
             {cancel, {UID, _CB}},
             empty,
             #{ reads := Reads, takes := Takes }
            ) ->
    NewReads = queue:filter(fun({ReadUID, _}) -> ReadUID =/= UID end, Reads),
    NewTakes = queue:filter(fun({TakeUID, _}) -> TakeUID =/= UID end, Takes),
    {keep_state, #{ reads => NewReads, takes => NewTakes}, {reply, From, unit}};
handle_event({call, From},
             {cancel, {UID, {_NewValue, _CB}}},
             {filled, _Value},
             #{ puts := Puts }
            ) ->
    NewPuts = queue:filter(fun({PutUID, {_, _}}) -> PutUID =/= UID end, Puts),
    {keep_state, #{ puts => NewPuts }, {reply, From, unit}};
handle_event({call, From}, {cancel, _}, {error, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, unit}};

handle_event({call, From},
             {kill, #{ left := Left }, Error},
             empty,
             #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_UID, Read}) -> spawn(Read(Left(Error))) end, ReadCBs),
    TakeCBs = queue:to_list(Takes),
    lists:foreach(fun({_UID, Take}) -> spawn(Take(Left(Error))) end, TakeCBs),
    {next_state, {killed, Error}, data, {reply, From, unit}};
handle_event({call, From},
             {kill, #{ left := Left }, Error},
             {filled, _Value},
             #{ puts := Puts }
            ) ->
    PutCBs = queue:to_list(Puts),
    lists:foreach(fun({_UID, {_, Put}}) -> spawn(Put(Left(Error))) end, PutCBs),
    {next_state, {killed, Error}, data, {reply, From, unit}};
handle_event({call, From}, {kill, _Util, _NewError}, {killed, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, unit}};

handle_event(cast,
             {put, #{ right := Right }, {_PutUID, {Value, CB}}},
             empty,
             Data = #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_ReadUID, Read}) -> spawn(Read(Right(Value))) end, ReadCBs),
    case queue:out(Takes) of
        {{value, {_TakeUID, Take}}, NewTakes} ->
            spawn(Take(Right(Value))),
            spawn(CB(Right(unit))),
            NewData = maps:put(takes, NewTakes, Data),
            {keep_state, NewData};
        {empty, Takes} ->
            spawn(CB(Right(unit))),
            {next_state, {filled, Value}, filled_queues()}
    end;
handle_event(cast, {put, _Util, CB}, {filled, _Value}, #{ puts := Puts }) ->
    NewData = #{ puts => queue:in(CB, Puts) },
    {keep_state, NewData};
handle_event(cast,
             {put, #{ left := Left }, {_UID, {_Value, CB}}},
             {killed, Error},
             _Data
            ) ->
    spawn(CB(Left(Error))),
    keep_state_and_data;

handle_event(cast, {read, _Util, CB}, empty, Data = #{ reads := Reads }) ->
    NewData = maps:put(reads, queue:in(CB, Reads), Data),
    {keep_state, NewData};
handle_event(cast, {read, #{ right := Right }, {_UID, CB}}, {filled, Value}, _Data) ->
    spawn(CB(Right(Value))),
    {next_state, empty, empty_queues()};
handle_event(cast, {read, #{ left := Left }, {_UID, CB}}, {killed, Error}, _Data) ->
    spawn(CB(Left(Error))),
    keep_state_and_data;

handle_event({call, From}, {status, Util}, State, _Data) ->
    {keep_state_and_data, {reply, From, handle_status(Util, State)}};

handle_event(cast, {take, _Util, CB}, empty, Data = #{ takes := Takes }) ->
    NewData = maps:put(takes, queue:in(CB, Takes), Data),
    {keep_state, NewData};
handle_event(cast,
             {take, #{ right := Right }, {_TakeUID, CB}},
             {filled, Value},
             #{ puts := Puts }
            ) ->
    spawn(CB(Right(Value))),
    case queue:out(Puts) of
        {{value, {_UID, {NewValue, Put}}}, NewPuts} ->
            spawn(Put(Right(NewValue))),
            NewData = #{ puts => NewPuts },
            {next_state, {filled, NewValue}, NewData};
        {empty, Puts} ->
            {next_state, empty, empty_queues()}
    end;
handle_event(cast, {take, #{ left := Left }, {_TakeUID, CB}}, {killed, Error}, _Data) ->
    spawn(CB(Left(Error))),
    keep_state_and_data;

handle_event({call, From},
             {tryPut, #{ right := Right }, Value},
             empty,
             #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_UID, Read}) -> spawn(Read(Right(Value))) end, ReadCBs),
    case queue:out(Takes) of
        {{value, {_UID, Take}}, NewTakes} ->
            spawn(Take(Right(Value))),
            NewData = #{ reads => Reads, takes => NewTakes},
            {keep_state, NewData, {reply, From, true}};
        {empty, Takes} ->
            {next_state, {filled, Value}, filled_queues(), {reply, From, true}}
    end;
handle_event({call, From}, {tryPut, _Util, _NewValue}, _State, _Data) ->
    {keep_state_and_data, {reply, From, false}};

handle_event({call, From}, {tryRead, Util}, State, _Data) ->
    {keep_state_and_data, {reply, From, handle_try_read(Util, State)}};

handle_event({call, From},
             {tryTake, #{ just := Just, right := Right }},
             {filled, Value},
             #{ puts := Puts}
            ) ->
    case queue:out(Puts) of
        {{value, {_UID, {NewValue, Put}}}, NewPuts} ->
            spawn(Put(Right(NewValue))),
            NewData = #{ puts => NewPuts },
            {next_state, {filled, NewValue}, NewData, {reply, From, Just(Value)}};
        {empty, Puts} ->
            {next_state, empty, empty_queues(), {reply, From, Just(Value)}}
    end;
handle_event({call, From}, {tryTake, #{ nothing := Nothing }}, _State, _Data) ->
    {keep_state_and_data, {reply, From, Nothing}}.

handle_status(#{ empty := Empty }, empty) -> Empty;
handle_status(#{ filled := Filled }, {filled, Value}) -> Filled(Value);
handle_status(#{ killed := Killed }, {killed, Error}) -> Killed(Error).

handle_try_read(#{ just := Just }, {filled, Value}) -> Just(Value);
handle_try_read(#{ nothing := Nothing }, _State) -> Nothing.

%% Cancellers

unique_canceller(AVar, CB) ->
    UniqCB = {erlang:unique_integer(), CB},
    Canceller = fun() -> gen_statem:call(AVar, {cancel, UniqCB}) end,
    {UniqCB, Canceller}.

%% Helpers

empty_queues() -> #{ reads => queue:new(), takes => queue:new() }.

filled_queues() -> #{ puts => queue:new() }.
