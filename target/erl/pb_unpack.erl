-module(pb_unpack).
-author("wanghaohao").
-compile(export_all).

-define(I32(V), pb:rint32(V)).
-define(I64(V), pb:rint64(V)).
-define(B(V), pb:rbool(V)).
-define(S(V), pb:rstring(V)).
-define(T(V, Type), pb:rtuple(V, Type)).
-define(L(V, Type), pb:rlist(V, Type)).


m_role_add_tos(B0) -> 
   {V1, B1}=?S(B0),
   {V2, B2}=?L(B1, int32),
   {V3, B3}=?B(B2),
   {V4, B4}=?L(B3, p_role_info),
   {{m_role_add_tos,V1,V2,V3,V4}, B4}.
p_role_info(B0) -> 
   {V1, B1}=?64(B0),
   {{p_role_info,V1}, B1}.
m_role_choose_tos(B0) -> 
   {V1, B1}=?I32(B0),
   {{m_role_choose_tos,V1}, B1}.
m_chat_auth_tos(B0) -> 
   {V1, B1}=?L(B0, p_role_info),
   {{m_chat_auth_tos,V1}, B1}.
m_chat_msg_tos(B0) -> 
   {V1, B1}=?S(B0),
   {{m_chat_msg_tos,V1}, B1}.
