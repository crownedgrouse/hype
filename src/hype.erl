%%%-----------------------------------------------------------------------------
%%% File:      hype.erl
%%% @author    Eric Pailleau <hype@crownedgrouse.com>
%%% @copyright 2020 crownedgrouse.com
%%% @doc
%%% Handsome YAML Parser for Erlang
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

-record(hype, {depth     = -1    :: integer() % internal use for structure depth indentation
			  ,canonical = false :: boolean() % use canonical representation
			  ,version   = true  :: boolean() % show yaml version
			  ,types     = false :: boolean() % declare erlang types TAG ! tag:github.com/crownedgrouse/hype,2020:
			  ,tag       = false :: boolean() % tag yaml type such as omap, set, binary, str 
			  ,flow      = true  :: boolean() % use flow format, more compact
			  ,width     = 80    :: integer() % fold string and binary to this width
			  ,indent    = "   " :: string()  % indentation to be used
			  ,footer    = true  :: boolean() % add ... footer or not
	   		  }).
%%-----------------------------------------------------------------------------
%% @doc 
%% @end
%%-----------------------------------------------------------------------------
offset(Rec)
	when is_record(Rec, hype)
	-> string:copies(Rec#hype.indent, Rec#hype.depth).

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
	     }.


print(Term) -> print(Term, []).
	

print(Term, Opt)
	-> case encode(Term, Opt) of
			{ok, D}    -> io:format("~ts", [D]);
			X -> throw(X)
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
		S = structure(Term),
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
structure(Term)
	when is_atom(Term)
	-> {atom, erlang:atom_to_list(Term)};
structure(Term)
	when is_binary(Term)
	-> {binary, Term};
structure(Term)
	when is_boolean(Term)
	-> {boolean, Term};
structure(Term)
	when is_float(Term)
	-> {float, Term};
structure(Term)
	when is_function(Term)
	-> {function, Term};
structure(Term)
	when is_integer(Term)
	-> {integer, Term};
structure(Term)
	when is_pid(Term)
	-> {pid, io_lib:format("~p", Term)};
structure(Term)
	when is_port(Term)
	-> {port, io_lib:format("~p", Term)};
structure(Term)
	when is_reference(Term)
	-> {reference, io_lib:format("~p", Term)};
%% Higher types
structure(Term)
	when is_tuple(Term)
	-> 
	Fun = fun(X) -> [structure(X)] end,
	Data = lists:flatmap(Fun, erlang:tuple_to_list(Term)),
	{tuple, Data};
structure(Term)
	when is_map(Term)
	-> 
	Fun = fun(X) -> [structure(X)] end,
	Data = lists:flatmap(Fun, maps:to_list(Term)),
	{map, Data};
structure(Term)
	when is_list(Term)
	-> 
	% Check if it is a string or a list of types
	case io_lib:printable_list(Term) of
		true  -> {string, Term};
		false -> 
			Fun = fun(X) -> [structure(X)] end,
			Data = lists:flatmap(Fun, Term),
			{list, Data}
	end;
%% Error unhandled type
structure(Term)
	-> 
	throw({unexpected, Term}).

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

presentation({binary, B}, Rec)    % Binary
	-> tag(binary, Rec),
	   Enc = encoding(B),
	   Raw = fold(Enc,Rec),
	   indent(Raw, Rec).

%%-----------------------------------------------------------------------------
%% @doc Add YAML tags if necessary
%% @end
%%-----------------------------------------------------------------------------
tag(_, _) -> [].

%%-----------------------------------------------------------------------------
%% @doc Verify or fix encoding
%% @end
%%-----------------------------------------------------------------------------
encoding(B)
	when is_binary(B)
	->  erlang:binary_to_list(B);
encoding(B) -> B.

%%-----------------------------------------------------------------------------
%% @doc Fold data if necessary
%% @end
%%-----------------------------------------------------------------------------
fold(R, _ ) -> R.

%%-----------------------------------------------------------------------------
%% @doc Indent data depending depth
%% @end
%%-----------------------------------------------------------------------------
indent(L, Rec) 
	when is_list(L)
	-> case io_lib:printable_list(L) of
			false -> lists:flatmap(fun(E) -> [offset(Rec) ++ [E]] end, L);
			true  -> offset(Rec) ++ [L]
		end;
indent(L, Rec)
	-> offset(Rec) ++ [L].

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
	string:strip(string:chomp(V), right) ++ io_lib:nl().

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