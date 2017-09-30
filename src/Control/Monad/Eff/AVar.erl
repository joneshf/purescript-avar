-module(control_monad_eff_aVar@foreign).
-export(['_killVar'/0, '_putVar'/0, '_readVar'/0, '_status'/0, '_takeVar'/0, '_tryPutVar'/0, '_tryReadVar'/0, '_tryTakeVar'/0, makeEmptyVar/0, makeVar/1]).

'_killVar'() ->
    fun(_Util, _Error, _AVar) ->
        '_killVar'
    end.

'_putVar'() ->
    fun(_Util, _Value, _AVar, _CB) ->
        '_putVar'
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
    fun(_Util, _AVar, _CB) ->
        '_takeVar'
    end.

'_tryPutVar'() ->
    fun(_Util, _Value, _AVar) ->
        '_tryPutVar'
    end.

'_tryReadVar'() ->
    fun(Util, AVar) ->
        fun() ->
            rpc(AVar, {tryRead, Util})
        end
    end.

'_tryTakeVar'() ->
    fun(_Util, _AVar) ->
        '_tryTakeVar'
    end.

makeEmptyVar() ->
    fun() ->
        makeEmptyVar
    end.

makeVar(Value) ->
    fun() ->
        spawn(fun() -> filled(Value) end)
    end.

%% AVar states

filled(Value) ->
    receive
        {From, {tryRead, #{ just := Just }}} ->
            From ! {self(), Just(Value)},
            filled(Value);

        Any ->
            io:format("[filled] Received: ~p~n", [Any]),
            filled(Value)
    end.

%% RPC framework

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
