%%%-------------------------------------------------------------------
%%% @author wanghaohao
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十月 2015 09:20
%%%-------------------------------------------------------------------
-module(pb).
-author("wanghaohao").

-define(ModuleID,(MethodID div 100)).

-include("common.hrl").
%% -include("all_pb.hrl").
%% -include("mm_define.hrl").

%% API
-export([unpack/1, pack/1]).
-export([rint32/1, rint64/1, rbool/1, rstring/1, rtuple/2, rlist/2, rdouble/1]).
-export([wint32/1, wint64/1, wbool/1, wstring/1, wtuple/2, wlist/2, wdouble/1]).

-compile(export_all).

-define( STRING(Str), (if is_list(Str) -> Str; is_binary(Str) -> binary_to_list(Str); true -> Str end)).

unpack(<<Unique:16, 1:1, MethodID:31, Bin/binary>>) ->
  Fun = pb_map:m_to_f(MethodID),
  {Tos, _} = pb_unpack:Fun(zlib:uncompress(Bin)),
  {Unique, ?ModuleID, MethodID, Tos};
unpack(<<Unique:16, MethodID:32, Bin/binary>>) ->
  Fun = pb_map:m_to_f(MethodID),
  {Tos, _} = pb_unpack:Fun(Bin),
  {Unique, ?ModuleID, MethodID, Tos}.

pack(R) ->
  Class = element(1, R),
  MethodID = pb_map:f_to_m(Class),
  Bin = pb_pack:Class(R),
  if
    byte_size(Bin) > 2048 ->
      Iscompress = 1,
      Bin1 = zlib:compress(Bin);
    true ->
      Iscompress = 0,
      Bin1 = Bin
  end,
  <<Iscompress:1, MethodID:31, Bin1/binary>>.

rint32(<<I:32/signed, B/binary>>) ->
  {I, B}.

rint64(<<I:64/signed, B/binary>>) ->
  {I, B}.

rdouble(B) ->
  rint64(B).

rbool(<<0:8, B/binary>>) ->
  {false, B};
rbool(<<_:8, B/binary>>) ->
  {true, B}.

rstring(<<Len:16, I:Len/binary-unit:8, B/binary>>) ->
  {?STRING(I), B}.

rtuple(<<0, B/binary>>, _Type) ->
  {undefined, B};
rtuple(<<1, B/binary>>, Type) ->
  pb_unpack:Type(B).

rlist(<<Len:16, B/binary>>, Type) ->
  {List,LeftBin} = case Type of
    int32 ->
      lists:foldl(fun(_, {Acc, Bin}) ->
        {I, Bin1} = rint32(Bin),
        {[I|Acc], Bin1}
      end, {[], B}, lists:seq(1, Len));
    int64 ->
      lists:foldl(fun(_, {Acc, Bin}) ->
        {I, Bin1} = rint64(Bin),
        {[I|Acc], Bin1}
      end, {[], B}, lists:seq(1, Len));
    bool ->
      lists:foldl(fun(_, {Acc, Bin}) ->
        {I, Bin1} = rbool(Bin),
        {[I|Acc], Bin1}
      end, {[], B}, lists:seq(1, Len));
    string ->
      lists:foldl(fun(_, {Acc, Bin}) ->
        {I, Bin1} = rstring(Bin),
        {[I|Acc], Bin1}
      end, {[], B}, lists:seq(1, Len));
    _ ->
      lists:foldl(fun(_, {Acc, Bin}) ->
        {I, Bin1} = rtuple(Bin, Type),
        {[I|Acc], Bin1}
      end, {[], B}, lists:seq(1, Len))
  end,
  {lists:reverse(List), LeftBin}.

wint32(V) when is_integer(V)->
  <<V:32/signed>>;
wint32(_) ->
  <<0:32/signed>>.

wint64(V) when is_integer(V) ->
  <<V:64/signed>>;
wint64(_) ->
  <<0:64/signed>>.

wdouble(V) ->
  wint64(V).

wbool(V) ->
  case V =:= false orelse V =:= 0 of
    true ->
      <<0>>;
    false ->
      <<1>>
  end.

wstring(V) ->
  Bin = if
    V =:= undefined -> <<>> ;
    is_list(V)      -> list_to_binary(V);
    is_integer(V)   -> list_to_binary(integer_to_list(V));
    is_atom(V)      -> atom_to_binary(V,latin1);
    is_tuple(V)     -> list_to_binary(tuple_to_list(V));
    is_bitstring(V) -> list_to_binary(bitstring_to_list(V));
    true               -> V
  end,
  <<(size(Bin)):16,Bin/binary>>.

wtuple(undefined, _Type) ->
  <<0>>;
wtuple(V, Type) ->
  <<1,(pb_pack:Type(V))/binary>>.




wlist(undefined, Type) ->
  wlist([], Type);
wlist(List, Type) when is_list(List) ->
  Len = length(List),
  Bin = lists:foldl(fun(V, Acc) ->
    <<Acc/binary, (witem(V, Type))/binary>>
  end, <<>>, List),
  <<Len:16, Bin/binary>>.

witem(V, Type) ->
  case Type of
    int32 ->
      wint32(V);
    int64 ->
      wint64(V);
    double ->
      wint64(V);
    bool ->
      wbool(V);
    string ->
      wstring(V);
    _ ->
      wtuple(V, Type)
  end.




















