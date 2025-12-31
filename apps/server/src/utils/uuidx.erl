-module(uuidx).
-author("Oleg G.Kapranov <lugatex@yahoo.com>").

-export([to_string/1
        ,uuid_v4/0
        ,v4/4
        ]).

-define(VARIANT10,2#10).
-define(UUIDv4,4).

-spec uuid_v4() -> binary().
uuid_v4() ->
  <<U0:32,U1:16,_:4,U2:12,_:2,U3:30,U4:32>> = crypto:strong_rand_bytes(16),
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b"
                             ,get_binary_uuid(<<U0:32,U1:16,?UUIDv4:4,U2:12,?VARIANT10:2,U3:30,U4:32>>))).

-spec to_string(binary()) -> binary().
to_string(U) ->
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",get_parts(U))).

% R1 => <<1111111111111:48>> - <<1111111111111111111111111111111111111111111111119:48>>.
% R2 => <<17:12>>            - <<1111111111119:12>>.
% R3 => <<111111119:32>>     - <<11111111111111111111111111111119:32>>.
% R4 => <<11111111:30>>      - <<11111111111111111111111111119:30>>.

-spec v4(integer(),integer(),integer(),integer()) -> binary().
v4(R1,R2,R3,R4) -> <<R1:48,4:4,R2:12,2:2,R3:32,R4:30>>.

-spec get_binary_uuid(binary()) -> list().
get_binary_uuid(<<TL:32,TM:16,THV:16,CSR:8,CSL:8,N:48>>) -> [TL,TM,THV,CSR,CSL,N].


-spec get_parts(binary()) -> list().
get_parts(<<TL:32,TM:16,THV:16,CSR:8,CSL:8,N:48>>) -> [TL,TM,THV,CSR,CSL,N].
