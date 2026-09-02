-module(erl_kernel_filename@foreign).

-export([ compareBinaryImpl/5
        , stringToBinary/1
        , binaryToStringImpl/3
        ]).

compareBinaryImpl(LT, EQ, GT, A, B) ->
  if A < B -> LT;
     A > B -> GT;
     true -> EQ
  end.

%% A purerl String is already a UTF-8 binary; this is the identity, named so the
%% PureScript side does not have to assert it with unsafeCoerce.
stringToBinary(S) -> S.

binaryToStringImpl(Just, Nothing, Bin) ->
  case unicode:characters_to_binary(Bin, utf8, utf8) of
    Decoded when is_binary(Decoded) -> Just(Decoded);
    _ -> Nothing
  end.
