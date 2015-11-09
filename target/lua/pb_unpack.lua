module("pb_unpack", package.seeall)
local I32 = pb.rint32
local I64 = pb.rint64
local B = pb.rbool
local S = pb.rstring
local T = pb.rtuple
local L = pb.rlist

function p_role_info(m)
   m.role_id=I64()
end
function m_role_add_toc(m)
   m.succ=B()
   m.reason=T(string)
end
function m_role_choose_toc(m)
   m.succ=B()
   m.reason=T(string)
end
function m_chat_msg_toc(m)
end
