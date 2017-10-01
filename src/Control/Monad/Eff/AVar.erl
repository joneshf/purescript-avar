-module(control_monad_eff_aVar@foreign).
-behavior(gen_statem).
-export(['_killVar'/0, '_putVar'/0, '_readVar'/0, '_status'/0, '_takeVar'/0, '_tryPutVar'/0, '_tryReadVar'/0, '_tryTakeVar'/0, makeEmptyVar/0, makeVar/1]).
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
            gen_statem:call(AVar, {put, Util, Value, CB})
        end
    end.

'_readVar'() ->
    fun(Util, AVar, CB) ->
        fun() ->
            gen_statem:call(AVar, {read, Util, CB})
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
            gen_statem:call(AVar, {take, Util, CB})
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

empty_queues() -> #{ reads => queue:new(), takes => queue:new() }.

filled_queues() -> #{ puts => queue:new() }.

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
             {cancel, {UID, _NewValue, _CB}},
             {filled, _Value},
             #{ puts := Puts }
            ) ->
    NewPuts = queue:filter(fun({PutUID, _, _}) -> PutUID =/= UID end, Puts),
    {keep_state, #{ puts => NewPuts }, {reply, From, unit}};
handle_event({call, From}, {cancel, _}, {error, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, unit}};

handle_event({call, From},
             {kill, #{ left := Left }, Error},
             empty,
             #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_UID, Read}) -> (Read(Left(Error)))() end, ReadCBs),
    TakeCBs = queue:to_list(Takes),
    lists:foreach(fun({_UID, Take}) -> (Take(Left(Error)))() end, TakeCBs),
    {next_state, {killed, Error}, data, {reply, From, unit}};
handle_event({call, From},
             {kill, #{ left := Left }, Error},
             {filled, _Value},
             #{ puts := Puts }
            ) ->
    PutCBs = queue:to_list(Puts),
    lists:foreach(fun({_UID, _, Put}) -> (Put(Left(Error)))() end, PutCBs),
    {next_state, {killed, Error}, data, {reply, From, unit}};
handle_event({call, From}, {kill, _Util, _NewError}, {killed, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, unit}};

handle_event({call, From},
             {put, #{ right := Right }, Value, CB},
             empty,
             Data = #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_UID, Read}) -> (Read(Right(Value)))() end, ReadCBs),
    case queue:out(Takes) of
        {{value, {_UID, Take}}, NewTakes} ->
            (Take(Right(Value)))(),
            (CB(Right(unit)))(),
            NewData = maps:put(takes, NewTakes, Data),
            {keep_state, NewData, {reply, From, unit_canceller()}};
        {empty, Takes} ->
            (CB(Right(unit)))(),
            {next_state, {filled, Value}, filled_queues(), {reply, From, unit_canceller()}}
    end;
handle_event({call, From},
             {put, _Util, NewValue, CB},
             {filled, _Value},
             #{ puts := Puts }
            ) ->
    {UniqCB, Canceller} = unique_canceller(NewValue, CB),
    NewData = #{ puts => queue:in(UniqCB, Puts) },
    {keep_state, NewData, {reply, From, Canceller}};
handle_event({call, From},
             {put, #{ left := Left }, _Value, CB},
             {killed, Error},
             _Data
            ) ->
    (CB(Left(Error)))(),
    {keep_state_and_data, {reply, From, unit_canceller()}};

handle_event({call, From}, {read, _Util, CB}, empty, Data = #{ reads := Reads }) ->
    {UniqCB, Canceller} = unique_canceller(CB),
    NewData = maps:put(reads, queue:in(UniqCB, Reads), Data),
    {keep_state, NewData, {reply, From, Canceller}};
handle_event({call, From},
             {read, #{ right := Right }, CB},
             {filled, Value},
             #{ puts := Puts }
            ) ->
    (CB(Right(Value)))(),
    case queue:out(Puts) of
        {{value, {_UID, NewValue, Put}}, NewPuts} ->
            (Put(Right(NewValue)))(),
            NewData = #{ puts => NewPuts },
            {next_state, {filled, NewValue}, NewData, {reply, From, unit_canceller()}};
        {empty, Puts} ->
            {next_state, empty, empty_queues(), {reply, From, unit_canceller()}}
    end;
handle_event({call, From}, {read, #{ left := Left }, CB}, {killed, Error}, _Data) ->
    (CB(Left(Error)))(),
    {keep_state_and_data, {reply, From, unit_canceller()}};

handle_event({call, From}, {status, Util}, State, _Data) ->
    {keep_state_and_data, {reply, From, handle_status(Util, State)}};

handle_event({call, From}, {take, _Util, CB}, empty, Data = #{ takes := Takes }) ->
    {UniqCB, Canceller} = unique_canceller(CB),
    NewData = maps:put(takes, queue:in(UniqCB, Takes), Data),
    {keep_state, NewData, {reply, From, Canceller}};
handle_event({call, From}, {take, #{ right := Right }, CB}, {filled, Value}, _Data) ->
    (CB(Right(Value)))(),
    {next_state, empty, empty_queues(), {reply, From, unit_canceller()}};
handle_event({call, From}, {take, #{ left := Left }, CB}, {killed, Error}, _Data) ->
    (CB(Left(Error)))(),
    {keep_state_and_data, {reply, From, unit_canceller()}};

handle_event({call, From},
             {tryPut, #{ right := Right }, Value},
             empty,
             #{ reads := Reads, takes := Takes }
            ) ->
    ReadCBs = queue:to_list(Reads),
    lists:foreach(fun({_UID, Read}) -> (Read(Right(Value)))() end, ReadCBs),
    case queue:out(Takes) of
        {{value, {_UID, Take}}, NewTakes} ->
            (Take(Right(Value)))(),
            NewData = #{ reads => Reads, takes => NewTakes},
            {keep_state, NewData, {reply, From, true}};
        {empty, Takes} ->
            {next_state, {filled, Value}, filled_queues(), {reply, From, true}}
    end;
handle_event({call, From}, {tryPut, _Util, _NewValue}, {filled, _Value}, _Data) ->
    {keep_state_and_data, {reply, From, false}};
handle_event({call, From}, {tryPut, _Util, _NewValue}, {killed, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, false}};

handle_event({call, From}, {tryRead, Util}, State, _Data) ->
    {keep_state_and_data, {reply, From, handle_try_read(Util, State)}};

handle_event({call, From}, {tryTake, #{ nothing := Nothing }}, empty, _Data) ->
    {keep_state_and_data, {reply, From, Nothing}};
handle_event({call, From},
             {tryTake, #{ just := Just, right := Right }},
             {filled, Value},
             #{ puts := Puts}
            ) ->
    case queue:out(Puts) of
        {{value, {_UID, NewValue, Put}}, NewPuts} ->
            (Put(Right(NewValue)))(),
            NewData = #{ puts => NewPuts },
            {next_state, {filled, NewValue}, NewData, {reply, From, Just(Value)}};
        {empty, Puts} ->
            {next_state, empty, empty_queues(), {reply, From, Just(Value)}}
    end;
handle_event({call, From}, {tryTake, #{ nothing := Nothing }}, {error, _Error}, _Data) ->
    {keep_state_and_data, {reply, From, Nothing}}.

handle_status(#{ empty := Empty }, empty) -> Empty;
handle_status(#{ filled := Filled }, {filled, Value}) -> Filled(Value);
handle_status(#{ killed := Killed }, {killed, Error}) -> Killed(Error).

handle_try_read(#{ nothing := Nothing }, empty) -> Nothing;
handle_try_read(#{ just := Just }, {filled, Value}) -> Just(Value);
handle_try_read(#{ nothing := Nothing }, {killed, _Error}) -> Nothing.

%% Cancellers

unique_canceller(CB) ->
    Self = self(),
    UniqCB = {erlang:unique_integer(), CB},
    Canceller = fun() -> gen_statem:call(Self, {cancel, UniqCB}) end,
    {UniqCB, Canceller}.

unique_canceller(Value, CB) ->
    Self = self(),
    UniqCB = {erlang:unique_integer(), Value, CB},
    Canceller = fun() -> gen_statem:call(Self, {cancel, UniqCB}) end,
    {UniqCB, Canceller}.

unit_canceller() -> fun() -> unit end.
