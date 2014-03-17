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
	 v64adrs/2,
	 v64adrs_async/1,
	 v64adrs_async/2]).

-spec hexdigit(0..15) -> byte().

hexdigit(Digit) when Digit >= 0, Digit =< 15 ->
  if Digit >= 10 ->
     Digit + ($a - 10);
  true ->
     Digit + $0
  end.

%% basic hex conversion

-type v6field() :: 0..65535.
-type v6addr() :: {v6field(), v6field(), v6field(), v6field(),
                   v6field(), v6field(), v6field(), v6field()}.

-spec v6hex(v6addr()) -> [byte()].

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

-spec v6hex_revarpa(v6addr()) -> string().

v6hex_revarpa(Addr) ->
    lists:append([
		  lists:append(lists:map(fun(D) -> [D, $.] end,
					 lists:reverse(v6hex(Addr)))),
		  "in6.arpa"]).

%% Conversion to IPv6 canonical format address

-spec v6hex_addr(v6addr()) -> string().

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

-spec v6hex_tuple(v6addr()) -> string().

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

-type dns_name() :: string().

%% from lib/kernel/src/inet_res.erl

-type dns_data() ::
        dns_name()
      | inet:ip4_address()
      | inet:ip6_address()
      | {MName :: dns_name(),
         RName :: dns_name(),
         Serial :: integer(),
         Refresh :: integer(),
         Retry :: integer(),
         Expiry :: integer(),
         Minimum :: integer()}
      | {inet:ip4_address(), Proto :: integer(), BitMap :: binary()}
      | {CpuString :: string(), OsString :: string()}
      | {RM :: dns_name(), EM :: dns_name()}
      | {Prio :: integer(), dns_name()}
      | {Prio :: integer(),Weight :: integer(),Port :: integer(),dns_name()}
      | {Order :: integer(),Preference :: integer(),Flags :: string(),
         Services :: string(),Regexp :: string(), dns_name()}
      | [string()]
      | binary().

-spec v64adrs(dns_name()) -> [dns_data()].

v64adrs(Name) ->
    v64adrs(Name, []).

-spec v64adrs(dns_name(), [dns_name()]) -> [dns_data()].

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

%% Caution: the following asynchronous code is
%% highly experimental.

%% The v64adrs_async/1 function
%% looks up the AAAA and AA RRsets for the given name
%% simultaneously by spawning the processes,
%% and wait for ?V6_TIMEOUT milliseconds to
%% the AAAA RRset *only*;
%% after the timeout it will return
%% the value found earlier in either AAAA or AA RRsets.

%% In v64adrs_async/2 the nameserver list (as in inet_res module)
%% is specified.

%% Is this timeout necessary?
-define(LOOKUP_TIMEOUT, 30000).

-spec v64adrs_async(dns_name()) -> [dns_data()].

v64adrs_async(Name) ->
    v64adrs_async(Name, []).

-spec v64adrs_async(dns_name(), [dns_name()]) -> [dns_data()].

v64adrs_async(Name, NS) ->
    NSL = case NS of
	      [] -> [];
	      _Else -> [{nameservers, _Else}]
	  end,
    Pid = self(),
    C1 = spawn_link(
	   fun() ->
		   Pid ! {self(), aaaa,
			  inet_res:lookup(Name, in, aaaa,
					  NSL, ?LOOKUP_TIMEOUT)} end),
    C2 = spawn_link(
	   fun() ->
		   Pid ! {self(), a,
			  inet_res:lookup(Name, in, a,
					  NSL, ?LOOKUP_TIMEOUT)} end),
    % For time in ?V6_TIMEOUT [ms]
    % Waiting for AAAA RR only
    receive
	{C1, aaaa, RR} when RR =/= [] ->
	    exit_and_discard(C2),
	    RR
    after
	?V6_TIMEOUT ->
	    wait_resolver(C1, C2, false, false)
    end.

% Wait for either A RR or AAAA RR
% and return which is received earlier
% (unless it is a null list)
% then kill the unnecessary process

-spec wait_resolver(pid(), pid(), boolean(), boolean()) -> dns_data().

wait_resolver(_P1, _P2, true, true) -> [];
wait_resolver(P1, P2, F1, F2) ->
    receive
	{P1, aaaa, []} -> % discard
	    wait_resolver(P1, P2, true, F2);
	{P2, a, []} -> % discard
	    wait_resolver(P1, P2, F1, true);
	{P1, aaaa, R1} when F1 == false ->
	    case F2 of
		true -> % no process message waiting
		    exit(P2, normal);
		false ->
		    ok = exit_and_discard(P2)
	    end,
	    R1;
	{P2, a, R2} when F2 == false ->
	    case F1 of
		true -> % no process message waiting
		    exit(P1, normal);
		false ->
		    ok = exit_and_discard(P1)
	    end,
	    R2
    end.

-spec exit_and_discard(pid()) -> ok | {error, {einval, term()}}.

exit_and_discard(P) ->
    exit(P, normal),
    receive
	{P, _Q, _R} -> % discard one message from the assigned process
	    ok;
	_Else -> % unexpected message
	    {error, {einval, _Else}}
    end.

%% end of module

%% License document: MIT license

%% Copyright (c) 2012, 2014 Kenji Rikitake.

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
