%%%-----------------------------------------------------------------------------
%%% File:      hype.erl
%%% @author    Eric Pailleau <hype@crownedgrouse.com>
%%% @copyright 2020 crownedgrouse.com
%%% @doc
%%% Handsome YAML Processor for Erlang
%%% @end
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2020-07-19
%%%-----------------------------------------------------------------------------
-module(hype).
-export([encode/1, encode/2, print/1, print/2]).

-define(YAML_VERSION, "1.2").
-define(PRESENT(Side, T, Rec), 
        erlang:display({T, Rec#hype.depth}),
        Tag = tag(T, Rec),
        erlang:display({tag, Tag}),
        Enc = encoding(B),
        erlang:display({enc, Enc}),
        Raw = fold(Enc,Rec),
        erlang:display({raw, Raw}),
        erlang:display({Side, right, Rec#hype.right}),
        indent(T, Side, Tag, Raw, comment(B, Rec), Rec)
       ).

-record(hype,	{depth     = -1    :: integer() % internal use for structure depth indentation
                ,right     = simple :: atom()    % internal use for presentation : empty, simple, complex
                ,canonical = false :: boolean() % use canonical representation
                ,version   = false :: boolean() % show yaml version
                ,types     = false :: boolean() % declare erlang types TAG ! tag:github.com/crownedgrouse/hype,2020:
                ,tag       = false :: boolean() % tag yaml type such as omap, set, binary, str 
                ,flow      = true  :: boolean() % use flow format, more compact
                ,width     = 80    :: integer() % fold string and binary to this width
                ,indent    = "   " :: string()  % indentation to be used
                ,footer    = false :: boolean() % add ... footer or not
                ,comment   = false :: boolean() % add comment on data
                ,records   = []    :: list()    % list of record definitions
                }).

%%-----------------------------------------------------------------------------
%% @doc Return the indentation string from depth
%% @end
%%-----------------------------------------------------------------------------
offset(Rec)
	when is_record(Rec, hype)
	-> string:copies(Rec#hype.indent, Rec#hype.depth).

%%-----------------------------------------------------------------------------
%% @doc Split a string in substrings of a given length
%% @end
%%-----------------------------------------------------------------------------
split(D, L)
    when is_list(D),is_integer(L)
    -> split(D, L, []). 

split(D, L, Acc)
    -> case ( length(D) =< L ) of
            true  ->  Acc ++ [D] ;
            false -> 
                ND   = string:sub_string(D, L + 1),
                NAcc = string:sub_string(D, 1, L),
                split(ND, L, Acc ++ [NAcc])
       end.

%%-----------------------------------------------------------------------------
%% @doc Test if a tuple is matching a declared record
%% @end
%%-----------------------------------------------------------------------------
-spec is_declared(any(), any()) -> boolean().

is_declared(Term, Rec)
    when is_tuple(Term), is_record(Rec, hype)
    -> 
    LT = erlang:tuple_to_list(Term),
    Key = hd(LT),
    case is_atom(Key) of
        false -> false ;
        true -> 
            L = Rec#hype.records,
            case lists:keyfind(Key, 1, L) of
                {Key, Def} 
                    when is_list(Def), (length(Def) =:= (length(LT) - 1))
                    -> true ;
                _   -> false
            end
    end;
is_declared(_, _) -> false.

right(D, Rec)
    when is_atom(D);is_binary(D);is_bitstring(D);is_boolean(D);is_float(D);is_function(D);is_integer(D);is_pid(D);is_port(D);is_reference(D)
    -> Rec#hype{depth = (Rec#hype.depth), right = simple};
right(_, Rec)
    -> Rec#hype{depth = (Rec#hype.depth + 1), right = complex}.

%%-----------------------------------------------------------------------------
%% @doc Initialize record with proplists options or default value
%% @end
%%-----------------------------------------------------------------------------
opt2rec(Opt)
	-> 
    R = #hype{}, % default values
    #hype{canonical = proplists:get_value(canonical, Opt, R#hype.canonical)
         ,version   = proplists:get_value(version,   Opt, R#hype.version)
         ,types     = proplists:get_value(types,     Opt, R#hype.types)
         ,tag       = proplists:get_value(tag,       Opt, R#hype.tag)
         ,flow      = proplists:get_value(flow,      Opt, R#hype.flow)
         ,width     = proplists:get_value(width,     Opt, R#hype.width)
         ,indent    = proplists:get_value(indent,    Opt, R#hype.indent)
         ,footer    = proplists:get_value(footer,    Opt, R#hype.footer)
         ,comment   = proplists:get_value(comment,   Opt, R#hype.comment)
         ,records   = proplists:get_value(records,   Opt, R#hype.records)
         }.


print(Term) -> print(Term, []).
	

print(Term, Opt)
    -> 
    try 
        case encode(Term, Opt) of
                {ok, D} when is_list(D) -> 
                    case io_lib:printable_list(D) of
                        true  ->  io:format("~ts", [D]);
                        false ->  throw(D)
                    end;
                X -> throw(X)
        end
    catch
        _:R -> {error, R}
    end.

%%-----------------------------------------------------------------------------
%% @doc Encode an Erlang Term to YAML
%% @end
%%-----------------------------------------------------------------------------
encode(Term)
	-> encode(Term, []).

encode(Term, Opt)
    -> 
    try 
    	S = structure(left, Term),
        erlang:display(S),
    	Rec = opt2rec(Opt),
    	{ok, presentation(S, Rec)}
    catch
    	_:R -> {error, R}
    end.


%%-----------------------------------------------------------------------------
%% @doc Detect structure and type
%% @end
%%-----------------------------------------------------------------------------

%% Lower types
structure(Side, Term)
	when is_atom(Term)
	-> {atom, Side, erlang:atom_to_list(Term), Term};
structure(Side, Term)
	when is_binary(Term)
	-> {binary, Side, Term, Term};
structure(Side, Term)
    when is_bitstring(Term) % incomplete number of bytes
    -> {bitstring, Side, Term, Term};
structure(Side, Term)
	when is_boolean(Term)
	-> {boolean, Side, Term, Term};
structure(Side, Term)
	when is_float(Term)
	-> {float, Side, io_lib:format("~p",[Term]), Term};
structure(Side, Term)
	when is_function(Term)
	-> {function, Side, term_to_binary(Term), Term};
structure(Side, Term)
	when is_integer(Term)
	-> {integer, Side, lists:flatten(io_lib:format("~p",[Term])), Term};
structure(Side, Term)
	when is_pid(Term)
	-> {pid, Side, term_to_binary(Term), Term};
structure(Side, Term)
	when is_port(Term)
	-> {port, Side, term_to_binary(Term), Term};
structure(Side, Term)
	when is_reference(Term)
	-> {reference, Side, term_to_binary(Term), Term};
%% Higher types
structure(Side, Term)
	when is_tuple(Term)
	-> 
	Fun = fun(X) -> [structure(right, X)] end,
	Data = lists:flatmap(Fun, erlang:tuple_to_list(Term)),
	{tuple, Side, Data, Term};
structure(Side, Term)
	when is_map(Term)
	-> 
	Fun = fun(X) -> [structure(right,X)] end,
	Data = lists:flatmap(Fun, maps:to_list(Term)),
	{map, Side, Data, Term};
structure(Side, Term)
	when is_list(Term)
	-> 
    % Check if it is a string or a list of types
    case io_lib:printable_list(Term) of
        true  -> {string, Side, Term};
        false -> 
            Fun = fun(X) -> [structure(right,X)] end,
            Data = lists:flatmap(Fun, Term),
            {list, Side, Data, Term}
    end;
%% Error unhandled type
structure(Side, Term)
	-> 
	throw({unexpected, Side, Term}).

%%-----------------------------------------------------------------------------
%% @doc Present structured data to YAML format
%% @end
%%-----------------------------------------------------------------------------
presentation(Struc, Rec)
    when (Rec#hype.depth =:= -1)  % Main entry point
    ->
    H = header(Rec),
    P = presentation(Struc, Rec#hype{depth=0}), % Init depth to 0
    F = footer(Rec),
    lists:flatten(H ++ P ++ F);

% Simple types
presentation({atom, Side, B, _}, Rec)      % Atom
    -> ?PRESENT(Side, atom, Rec);
presentation({binary, Side, B, _}, Rec)    % Binary
    -> ?PRESENT(Side, binary, Rec);
presentation({float, Side, B, _}, Rec)     % Float
    -> ?PRESENT(Side, float, Rec);      
presentation({function, Side, B, _}, Rec)  % Fun
    -> ?PRESENT(Side, function, Rec);    
presentation({integer, Side, B, _}, Rec)   % Integer
    -> ?PRESENT(Side, integer, Rec);        
presentation({pid, Side, B, _}, Rec)       % Pid
    -> ?PRESENT(Side, pid, Rec);  
% Complex types
presentation({tuple, _, B, _}, Rec)               % Tuple {X} = tuple = unordered set with null value
    when is_list(B), (length(B) =:= 1)
    -> [X] = B,
       presentation(X, Rec#hype{right=empty});
presentation({tuple, _, B, _}, Rec)               % Tuple {X, Y} = property = omap
    when is_list(B), (length(B) =:= 2)
    -> [{A, _, C, D}, Y] = B,
       presentation({A, left, C, D}, Rec) ++ presentation(Y, right(Y,Rec));
presentation({tuple, Side, [X|_]= B, Term}, Rec)     % Tuple {X, Y, Z ...} = record = set or omap (if record declared)
    when is_list(B), (length(B) > 2)
    -> 
    Size = length(B),
    case X of
        {atom, RKT} -> % Maybe a record
            RecordTag = erlang:list_to_atom(RKT),
            case (is_record(Term, RecordTag, Size) or is_declared(Term, Rec)) of
                false  -> ?PRESENT(Side, tuple, Rec);
                true   -> ?PRESENT(Side, record, Rec)
            end;
        {_, _} -> % Not a record
                  ?PRESENT(Side, tuple, Rec)
    end;

presentation(_, _) -> throw(invalid_type).

%%-----------------------------------------------------------------------------
%% @doc Create header 
%% @end
%%-----------------------------------------------------------------------------
header(Rec) -> 
    T = case Rec#hype.types of
            true -> "TAG ! tag:github.com/crownedgrouse/hype,2020" ;
            false -> []
        end,
    V = case Rec#hype.version of
            true  -> io_lib:format("%YAML ~ts ~ts", [?YAML_VERSION, T]) ;
            false -> []
        end,
    H = case V of
            [] when Rec#hype.footer =:= true -> io_lib:format("---~ts", [io_lib:nl()]);
            [] -> [];
            _  ->  io_lib:format("~ts~ts---~ts", [V, io_lib:nl(), io_lib:nl()])
        end,
    case H of
        [] -> [];
        _  -> string:strip(string:chomp(H), right) ++ io_lib:nl()
    end.

%%-----------------------------------------------------------------------------
%% @doc Create footer
%% @end
%%-----------------------------------------------------------------------------
-spec footer(tuple()) -> list().

footer(Rec)
    when Rec#hype.footer =:= true
    -> io_lib:nl() ++ "..." ++ io_lib:nl();
footer(_)
    -> [].
%%-----------------------------------------------------------------------------
%% @doc Comment (only binary terms)
%% @end
%%-----------------------------------------------------------------------------
comment(D, Rec) 
    when Rec#hype.comment =:= true, is_binary(D)
    ->  case (catch erlang:binary_to_term(D)) of
            {'EXIT', _} -> [];
            T -> io_lib:format(" # ~p", [T])
        end;
comment(_, _ ) 
    -> [].

%%-----------------------------------------------------------------------------
%% @doc Add YAML tags if necessary
%%      'types=true' is overriding yaml basic types
%%      See http://erlang.org/doc/reference_manual/typespec.html#the-erlang-type-language
%% @end
%%-----------------------------------------------------------------------------
tag(atom, Rec)   when Rec#hype.types =:= true
    -> "!atom ";
tag(atom, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!str ";
tag(binary, Rec) when Rec#hype.types =:= true
    -> "!binary ";
tag(binary, Rec) when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!binary ";
tag(bitstring, Rec) when Rec#hype.types =:= true
    -> "!bitstring ";
tag(bitstring, Rec) when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!binary ";
tag(boolean, Rec)   when Rec#hype.types =:= true
    -> "!boolean ";
tag(boolean, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!bool ";
tag(float, Rec)   when Rec#hype.types =:= true
    -> "!float ";
tag(float, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!float ";
tag(function, Rec)   when Rec#hype.types =:= true
    -> "!function ";
tag(function, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!binary ";
tag(tuple, Rec)   when Rec#hype.types =:= true
    -> "!tuple ";
tag(tuple, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!set ";
tag(property, Rec)   when Rec#hype.types =:= true
    -> "!property ";
tag(property, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!omap ";
tag(record, Rec)   when Rec#hype.types =:= true
    -> "!record ";
tag(record, Rec)   when Rec#hype.tag =:= true; Rec#hype.canonical =:= true
    -> "!!omap ";
tag(_, _) -> [].

%%-----------------------------------------------------------------------------
%% @doc Encode to base64 when necessary
%% @end
%%-----------------------------------------------------------------------------
encoding(B)
    when is_binary(B)
    ->  erlang:binary_to_list(base64:encode(B));
encoding(B) -> B.

%%-----------------------------------------------------------------------------
%% @doc Fold data if necessary
%% @end
%%-----------------------------------------------------------------------------
fold(D, Rec)
    when ( length(D) > Rec#hype.width )
    -> split(D, Rec#hype.width) ;
fold(D, _) -> D.

%%-----------------------------------------------------------------------------
%% @doc Indent data depending depth
%% @end
%%-----------------------------------------------------------------------------
% indent(T, L, C, Rec) 
%     when is_list(L)
%     ->  case io_lib:printable_list(L) of
%             false when Rec#hype.canonical =:= false, 
%                        Rec#hype.tag =:= false
%                 -> [$|] ++ C ++ io_lib:nl() ++lists:flatmap(fun(E) -> [offset(Rec) ++ [E] ++ io_lib:nl()] end, L);
%             false when Rec#hype.canonical =:= false, 
%                        Rec#hype.tag =:= true
%                 -> T ++ [$|] ++ C ++ io_lib:nl() ++ lists:flatmap(fun(E) -> [offset(Rec) ++ [E] ++ io_lib:nl()] end, L);
%             false when Rec#hype.canonical =:= true
%                 -> T ++ [$", $\\] ++ io_lib:nl() ++ offset(Rec) ++ lists:join([$\\, io_lib:nl()], lists:flatmap(fun(E) -> [offset(Rec) ++ [E]] end, L)) ++ [$"];
%             true  when Rec#hype.tag =:= false 
%                 -> offset(Rec) ++ [L] ++ C ++ io_lib:nl() ;
%             true  when Rec#hype.tag =:= true 
%                 -> T ++ offset(Rec) ++ [L] ++ C ++ io_lib:nl()
%     	end;

indent(atom, left, T, L, C, Rec)
    when Rec#hype.right =:= empty
    -> offset(Rec) ++ T ++ [L] ++ C  ++ io_lib:nl() ;
indent(atom, left, T, L, _, Rec)
    when Rec#hype.right =:= simple
    -> offset(Rec) ++ T ++ [L] ++ ": ";
indent(atom, left, T, L, C, Rec)
    when Rec#hype.right =:= complex
    -> offset(Rec) ++ T ++ [L] ++ ": " ++ C  ++ io_lib:nl();
indent(atom, right, T, L, C, _Rec)
	->  T ++ [L] ++ C ++ io_lib:nl();
%indent(tuple, left, T, L, _, Rec)    ->

indent(A, S, T, L, C, Rec)
    -> throw({missing_type, A, S, T, L, C, Rec}).

