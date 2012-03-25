%% v6hex.erl:
%% IPv6-address tuple to hex string conversion
%% (and miscellaneous IPv6 goodies)
%% by Kenji Rikitake
%% MIT license (see the end of this module)

-module(v6hex).

-export([v6hex/1, 
	 v6hex_revarpa/1,
	 v6hex_addr/1,
	 v6hex_tuple/1,
	 v64adrs/1,
	 v64adrs/2]).

hexdigit(Digit) when Digit >= 0, Digit =< 15 ->
  if Digit >= 10 ->
     Digit + ($a - 10);
  true ->
     Digit + $0
  end.

%% basic hex conversion

v6hex({T0, T1, T2, T3, T4, T5, T6, T7})
  when T0 >= 0, T0 =< 65535, 
       T1 >= 0, T1 =< 65535, 
       T2 >= 0, T2 =< 65535, 
       T3 >= 0, T3 =< 65535, 
       T4 >= 0, T4 =< 65535, 
       T5 >= 0, T5 =< 65535, 
       T6 >= 0, T6 =< 65535, 
       T7 >= 0, T7 =< 65535 ->
    % make binaries from the integers
    % and split the binaries into the hex digit binaries
    <<A0:4, A1:4, A2:4, A3:4>> = <<T0:16/integer-unsigned-big>>,
    <<A4:4, A5:4, A6:4, A7:4>> = <<T1:16/integer-unsigned-big>>,
    <<A8:4, A9:4, A10:4, A11:4>> = <<T2:16/integer-unsigned-big>>,
    <<A12:4, A13:4, A14:4, A15:4>> = <<T3:16/integer-unsigned-big>>,
    <<A16:4, A17:4, A18:4, A19:4>> = <<T4:16/integer-unsigned-big>>,
    <<A20:4, A21:4, A22:4, A23:4>> = <<T5:16/integer-unsigned-big>>,
    <<A24:4, A25:4, A26:4, A27:4>> = <<T6:16/integer-unsigned-big>>,
    <<A28:4, A29:4, A30:4, A31:4>> = <<T7:16/integer-unsigned-big>>,
    % return canonical format of IPv6 address in plain 32-digit hex
    [hexdigit(A0), hexdigit(A1), hexdigit(A2), hexdigit(A3),
     hexdigit(A4), hexdigit(A5), hexdigit(A6), hexdigit(A7),
     hexdigit(A8), hexdigit(A9), hexdigit(A10), hexdigit(A11),
     hexdigit(A12), hexdigit(A13), hexdigit(A14), hexdigit(A15),
     hexdigit(A16), hexdigit(A17), hexdigit(A18), hexdigit(A19),
     hexdigit(A20), hexdigit(A21), hexdigit(A22), hexdigit(A23),
     hexdigit(A24), hexdigit(A25), hexdigit(A26), hexdigit(A27),
     hexdigit(A28), hexdigit(A29), hexdigit(A30), hexdigit(A31)].

%% Conversion to ip6.arpa reverse lookup name

v6hex_revarpa(Addr) ->
    lists:append([
		  lists:append(lists:map(fun(D) -> [D, $.] end,
					 lists:reverse(v6hex(Addr)))),
		  "in6.arpa"]).

%% Conversion to IPv6 canonical format address 

v6hex_addr(Addr) ->
    [C0, C1, C2, C3, C4, C5, C6, C7,
     C8, C9, C10, C11, C12, C13, C14, C15,
     C16, C17, C18, C19, C20, C21, C22, C23,
     C24, C25, C26, C27, C28, C29, C30, C31] = v6hex(Addr),
    [C0, C1, C2, C3, $:,
     C4, C5, C6, C7, $:, 
     C8, C9, C10, C11, $:,
     C12, C13, C14, C15, $:,
     C16, C17, C18, C19, $:,
     C20, C21, C22, C23, $:,
     C24, C25, C26, C27, $:,
     C28, C29, C30, C31].

%% Conversion to tuple-format string with hex-format elements

v6hex_tuple(Addr) ->
    [C0, C1, C2, C3, C4, C5, C6, C7,
     C8, C9, C10, C11, C12, C13, C14, C15,
     C16, C17, C18, C19, C20, C21, C22, C23,
     C24, C25, C26, C27, C28, C29, C30, C31] = v6hex(Addr),
    [${,
     $1, $6, $#, 
     C0, C1, C2, C3, $,,
     $1, $6, $#, 
     C4, C5, C6, C7, $,, 
     $1, $6, $#, 
     C8, C9, C10, C11, $,,
     $1, $6, $#, 
     C12, C13, C14, C15, $,,
     $1, $6, $#, 
     C16, C17, C18, C19, $,,
     $1, $6, $#, 
     C20, C21, C22, C23, $,,
     $1, $6, $#, 
     C24, C25, C26, C27, $,,
     $1, $6, $#, 
     C28, C29, C30, C31, $}].

%% The v64adrs/1 function first looks up the AAAA RRset of
%% the given name and wait for ?V6_TIMEOUT milliseconds;
%% if the AAAA RRset is found out that is returned.
%% when timed out it looks up the A RRset instead
%% and returns the lookup result.
%% In v64adrs/2 the nameserver list (as in inet_res module)
%% is specified.

-define(V6_TIMEOUT, 200).

v64adrs(Name) ->
    v64adrs(Name, []).

v64adrs(Name, NS) ->
    NSL = case NS of
	      [] -> [];
	      _Else -> [{nameservers, _Else}]
	  end,
    case (catch inet_res:lookup(Name, in, aaaa,
				NSL, ?V6_TIMEOUT)) of
	[] ->
	    inet_res:lookup(Name, in, a, NSL);
	_Ans -> _Ans
    end.

%% end of module

%% License document: MIT license

%% Copyright (c) 2012 Kenji Rikitake.

%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:

%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
