Nonterminals record namedrecord expr name_value name_values forms form.

Terminals '#' '{' '}' atom '=' '|' ',' dot .

Expect 2.

Rootsymbol forms.

forms -> form : ['$1'].
forms -> form forms : ['$1' | '$2'].

form -> record : '$1'.
form -> namedrecord : '$1'.
form -> record dot : '$1'.
form -> namedrecord dot : '$1'.

record -> '#' '{' name_values '}' : strip('$3').
namedrecord -> '#' atom '{' name_values '}': {strip('$2'), '$4'}.
name_values -> name_value : ['$1'].
name_values -> name_value ',' name_values : strip(['$1' | '$3']).
name_value -> atom '=' expr : {strip('$1'), strip('$3')}.
expr -> atom '|' expr : [strip('$1') | '$3' ].
expr -> atom :  [ strip('$1') ].

Erlang code.

strip({atom, _, Val}) ->
    Val;
strip([Val]) ->
    Val;
strip(Val) ->
    Val.

