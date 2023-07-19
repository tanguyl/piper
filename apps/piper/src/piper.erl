-module(piper).

-export([start_link/0]).

start_link() ->
    Pid = spawn_link(fun loop/0),
    register(?MODULE, Pid),
    {ok, Pid}.


loop()->
    PipeName = "priv/pipe_file",
    format_priv("~nWaiting for pipe. ~n", []),
    {ok, Binary} = file:read_file(PipeName),
    {Types, Content} = lists:unzip(process(Binary)),
    format_priv("Read ~w~n", [Content]),
    % Inputs starting with an atom are considered to execute a blas.
    Result  = case Content of
        [Atom|_] when is_atom(Atom) -> format_priv("Executing blas~n", []), execute_blas(Content);
        _                            -> format_priv("Returning content as is~n", []), Content
    end,
    format_priv("Sent back ~w~n", [Result]),
    ok      = file:write_file(PipeName, encode(lists:zip(Types, Result))),
    loop().


format_priv(Format, Content)->
    {ok, File} = file:open("priv/log.txt", [append]),
    io:format(File, Format, Content),
    file:close(File).

encode(List)->
    encode(List, <<>>).

encode([], Binary) -> Binary;
encode([{T, H}|Tail], Binary) ->
    Encoded = 
    if
        T == 0 -> % Integer 
            <<32:32/native-integer, 0:32/native-integer, H:32/native-integer>>;

        T == 1 -> % String
            SB = atom_to_binary(H),
            Size = bit_size(SB),
            <<Size:32/native-integer, 1:32/native-integer, SB/binary >>;

        T == 2 -> % Array
            Bin = blas:to_bin(H),
            <<(bit_size(Bin)):32/native-integer, 2:32/native-integer, Bin/binary>>;

        T == 3 -> % Character
            <<8:32/native-integer, 3:32/native-integer, (atom_to_binary(H)):8/bitstring>>;

        T == 4 -> % Real
            <<32:32/native-integer, 4:32/native-integer, H:32/native-float>>;
        
        T == 5 -> % Double
            <<64:32/native-integer, 5:32/native-integer, H:64/native-float>>;

        T == 6 -> % Logical
            <<32:32/native-integer, 6:32/native-integer, H:32/native-integer>>;

        T == 7 -> % Complex
            <<64:32/native-integer, 7:32/native-integer, H:8/binary>>;
        
        T == 8 -> % complex Double
            <<128:32/native-integer, 8:32/native-integer, H:16/binary>>
    end,
    encode(Tail, <<Binary/binary, Encoded/binary>>).


process(Data)->
    process(Data, []).


process(<<>>, Processed)->
    lists:reverse(Processed);

process(Data, Processed)->
    << Size:32/native-integer, Type:32/native-integer, Rest/binary>> = Data,
    
    ok = format_priv("Processing ~w~n", [Data]),
    ok = format_priv("Extracting ~w bits of type ~w~n", [Size, Type]),
    
    {Decoded, NextData} =  case Type of 
        0 -> % Integer.
            <<Integer:32/native-integer, Next/binary>> = Rest,
            {Integer,Next};
        1 -> % String.
            <<String:Size/bitstring, Next/binary>> = Rest,
            {binary_to_atom(String), Next};
        2 -> % Array.
            <<Binary:(trunc(Size/8))/binary, Next/binary>> = Rest,
            Array = blas:new(Binary),
            {Array, Next};
        3 -> % Character:
            <<Char:Size/bitstring, Next/binary>> = Rest,
            CharAtom = list_to_atom(string:to_lower(binary_to_list(Char))),
            {CharAtom, Next};
        4 -> % Real.
            <<Real:32/native-float, Next/binary>> = Rest,
            {Real, Next};
        5 -> % Double.
            <<Double:64/native-float, Next/binary>> = Rest,
            {Double, Next};
        6 -> % Logical.
            <<Logical:32/native-integer, Next/binary>> = Rest,
            {Logical, Next};
        7 -> % Complex.
            <<Complex:(trunc(Size/8))/binary, Next/binary>> = Rest,
            {Complex, Next};
        8 -> % Complex16
            <<Complex:(trunc(Size/8))/binary, Next/binary>> = Rest,
            {Complex, Next}
    end,
    process(NextData, [{Type, Decoded}] ++ Processed).


execute_blas(Content)->
    % Content: name, arg1, arg2, Info
    % Info = blas:run({name, blasRowMajor, arg1, ...}).
    AllButLast = [Name| Args] = lists:droplast(Content),
    Command = list_to_tuple([Name] ++ [blasRowMajor] ++ Args ),
    Command = list_to_tuple(AllButLast ),

    AllButLast ++ case blas:run(Command) of
        ok -> [0];
        V  -> [V]
    end.