-module(control_monad_eff_aVar@foreign).
-export(['_killVar'/0, '_putVar'/0, '_readVar'/0, '_status'/0, '_takeVar'/0, '_tryPutVar'/0, '_tryReadVar'/0, '_tryTakeVar'/0, makeEmptyVar/0, makeVar/1]).

'_killVar'() ->
    fun(_Util, _Error, _AVar) ->
        '_killVar'
    end.

'_putVar'() ->
    fun(Util, Value, AVar, CB) ->
        fun() ->
            rpc(AVar, {put, Util, Value, CB})
        end
    end.

'_readVar'() ->
    fun(_Util, _AVar, _CB) ->
        '_readVar'
    end.

'_status'() ->
    fun(_Util, _AVar) ->
        '_status'
    end.

'_takeVar'() ->
    fun(Util, AVar, CB) ->
        fun() ->
            rpc(AVar, {take, Util, CB})
        end
    end.

'_tryPutVar'() ->
    fun(Util, Value, AVar) ->
        fun() ->
            rpc(AVar, {tryPut, Util, Value})
        end
    end.

'_tryReadVar'() ->
    fun(Util, AVar) ->
        fun() ->
            rpc(AVar, {tryRead, Util})
        end
    end.

'_tryTakeVar'() ->
    fun(Util, AVar) ->
        fun() ->
            rpc(AVar, {tryTake, Util})
        end
    end.

makeEmptyVar() ->
    fun() ->
        spawn(fun() -> empty(queue:new(), queue:new()) end)
    end.

makeVar(Value) ->
    fun() ->
        spawn(fun() -> filled(queue:new(), Value) end)
    end.

%% AVar states

empty(Reads, Takes) ->
    receive
        {From, {put, #{ right := Right }, Value, CB}} ->
            ReadCBs = queue:to_list(Reads),
            lists:foreach(fun(ReadCB) -> (ReadCB(Right(Value)))() end, ReadCBs),
            case queue:out(Takes) of
                {empty, _} ->
                    (CB(Right(Value)))(),
                    Canceller = fun() -> self() ! cancel, unit end,
                    From ! {self(), Canceller},
                    filled(queue:new(), Value);

                {{value, OldCB}, Tail} ->
                    (OldCB(Right(Value)))(),
                    (CB(Right(unit)))(),
                    Canceller = fun() -> self() ! cancel, unit end,
                    From ! {self(), Canceller},
                    empty(queue:new(), queue:in(CB, Tail))
            end;

        {From, {take, _Util, CB}} ->
            From ! {self(), fun() -> self() ! cancel, unit end},
            empty(Reads, queue:in(CB, Takes));

        {From, {tryPut, _Util, Value}} ->
            From ! {self(), true},
            filled(queue:new(), Value);

        {From, {tryRead, #{ nothing := Nothing }}} ->
            From ! {self(), Nothing},
            empty(Reads, Takes);

        {From, {tryTake, #{ nothing := Nothing }}} ->
            From ! {self(), Nothing},
            empty(Reads, Takes);

        Any ->
            io:format("[empty] Received: ~p~n", [Any]),
            empty(Reads, Takes)
    end.

filled(Puts, Value) ->
    receive
        {From, {take, #{ right := Right }, CB}} ->
            case queue:out(Puts) of
                {empty, _} ->
                    (CB(Right(Value)))(),
                    Canceller = fun() -> self() ! cancel, unit end,
                    From ! {self(), Canceller},
                    empty(queue:new(), queue:new());

                {{value, {OldCB, NewValue}}, Tail} ->
                    (OldCB(Right(unit)))(),
                    (CB(Right(Value)))(),
                    Canceller = fun() -> self() ! cancel, unit end,
                    From ! {self(), Canceller},
                    filled(Tail, NewValue)
            end;

        {From, {tryPut, _Util, _Value}} ->
            From ! {self(), false},
            filled(Puts, Value);

        {From, {tryRead, #{ just := Just }}} ->
            From ! {self(), Just(Value)},
            filled(Puts, Value);

        {From, {tryTake, #{ just := Just }}} ->
            From ! {self(), Just(Value)},
            empty(queue:new(), queue:new());

        Any ->
            io:format("[filled] Received: ~p~n", [Any]),
            filled(Puts, Value)
    end.

%% RPC framework

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
