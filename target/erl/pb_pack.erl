-module(pb_pack).
-author("wanghaohao").
-compile(export_all).

-define(I32(V), (pb:wint32(V))/binary).
-define(I64(V), (pb:wint64(V))/binary).
-define(B(V), (pb:wbool(V))/binary).
-define(S(V), (pb:wstring(V))/binary).
-define(T(V, Type), (pb:wtuple(V, Type))/binary).
-define(L(V, Type), (pb:wlist(V, Type))/binary).

p_role_info(_,V1) -> 
   <<?I64(V1)>>
m_role_add_toc(_,V1,V2) -> 
   <<?B(V1),?S(V2)>>
m_role_choose_toc(_,V1,V2) -> 
   <<?B(V1),?S(V2)>>
m_chat_msg_toc(_) -> 
   <<>>
