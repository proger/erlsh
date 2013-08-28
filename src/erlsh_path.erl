-module(erlsh_path).
-export([escape/1]).

escape(Path) ->
    R = reserved(),
    lists:append([char_encode(Char, R) || Char <- Path]).

reserved() ->
    sets:from_list([$/, $\\, $:, $%]).

char_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true ->
            [$% | integer_to_list(Char)];
        false ->
            [Char]
    end.
