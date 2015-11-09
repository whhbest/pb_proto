module("pb_pack", package.seeall)

local I32 = pb.wint32
local I64 = pb.wint64
local B = pb.wbool
local S = pb.wstring
local T = pb.wtuple
local L = pb.wlist

function m_role_add_tos(t)
   s=s..S(t.name)
   s=s..L(t.time, "int32")
   s=s..B(t.sex)
   s=s..L(t.role_info, "p_role_info")
   return s
end
function p_role_info(t)
   s=s..I64(t.role_id)
   return s
end
function m_role_choose_tos(t)
  s=s..I32(t.role_id)
   return s
end
function m_chat_auth_tos(t)
   s=s..L(t.role_infos, "p_role_info")
   return s
end
function m_chat_msg_tos(t)
   s=s..S(t.msg)
   return s
end
