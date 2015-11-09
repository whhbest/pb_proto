-module(pb_map).
-author("wanghaohao").
%% API
-export([m_to_f/1,
         f_to_m/1]).

m_to_f(101) ->   m_role_add_tos;
m_to_f(102) ->   m_role_choose_tos;
m_to_f(201) ->   m_chat_auth_tos;
m_to_f(202) ->   m_chat_msg_tos;
m_to_f(_) ->  undefined.

f_to_m(m_role_add_toc) ->  101;
f_to_m(m_role_choose_toc) ->  102;
f_to_m(m_chat_auth_toc) ->  201;
f_to_m(m_chat_msg_toc) ->  202;
f_to_m(_) ->  0.
