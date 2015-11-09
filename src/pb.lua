--- 协议解析
module("pb", package.seeall)

--- 用于保存接收到的网络数据用于解析
local base = 2^8
local unique = 0
local data = nil
local pos = 1

--- 生成unique
local function genUnique()
    return 1
end
-- local className = ""

local function readbyte()
    local b1 = string.byte(data, pos)
    return b1
end

local function read2byte()
    local b1 = string.byte(data, pos)
    local b2 = string.byte(data, pos+1)
    pos = pos + 2
    return b1 * base + b2
end

local function readu4byte()
    local b1 = string.byte(data, pos)
    local b2 = string.byte(data, pos+1)
    local b3 = string.byte(data, pos+2)
    local b4 = string.byte(data, pos+3)
    if b1 and b2 and b3 and b4 then
        pos = pos + 4
        local value = b1*(2^24)+b2*(2^16)+b3*(2^8)+b4
        return value
    end
    assert("read4byte error")
end

local function writebyte(byte)
    local tp = type(byte)
    if tp == "number" then
        assert(byte < 256 and byte > -128, "byte lager then 256")
        return string.char((byte+256) % 256)
    elseif tp == "string" then
        return string.sub(byte, 1, 1)
    else
        -- error("writeByte type error")
        return string.char(0)
    end
end

local function write2byte(bytes)
    local tp = type(bytes)
    if tp == "number" then
        assert(bytes < (2 ^ 16), "bytes larger then 2^16")
        return string.char(math.floor(bytes / base)) .. string.char(math.floor(bytes % base))
    elseif tp == "string" then
        assert(#bytes>=2, "bytes len smaller then 2")
        return string.sub(bytes, 1, 2)
    else
        return string.char(0) .. string.char(0)
        -- error("write2byte type error")
    end
end

local function writeu4byte(bytes)
    local tp = type(bytes)
    if tp == "number" then
        assert(bytes < (2^32), "bytes larger then 2^32")
        local strTmp = ""
        for i=1, 4 do
            strTmp = strTmp .. string.char(bytes % base)
            bytes = math.floor(bytes / base)
        end
        return string.reverse(strTmp)
    elseif tp == "string" then
        assert(#bytes >= 4, "bytes len smaller then 4")
        return string.sub(bytes, 1, 4)
    else
        return string.rep(string.char(0), 4)
        -- error("write4byte type error")
    end
end

function pack(data)
    local fun = data.class
    local unique = genUnique()
    local methodID = pb_map.f2m[fun]
    local bin = pb_pack[fun](data)
    if #bin > 2048 then
        methodID = 2^31+methodID
        local compress = zlib.deflate()
        bin = compress(bin, "finish")
    end
    return write2byte(unique) .. writeu4byte(methodID) .. bin
end

function unpack(bin)
    data = bin
    pos = 1
     -- local unique = read2byte()
    local methodID = readu4byte()
    if methodID >= 2 ^ 31 then
        methodID = methodID - 2 ^ 31
        local uncom = zlib.inflate()
        data = uncom(string.sub(bin, 5))
        pos = 1
    end
    local fun = pb_map.m2f[methodID]
    print("begin unpack proto: ", methodID, fun)
    local tuple = {class = fun}
    pb_unpack[fun](tuple)
    return tuple
end

function rint32()
    local b1 = string.byte(data, pos)
    local b2 = string.byte(data, pos+1)
    local b3 = string.byte(data, pos+2)
    local b4 = string.byte(data, pos+3)
    if b1 and b2 and b3 and b4 then
        pos = pos + 4
        local value = b1*(2^24)+b2*(2^16)+b3*(2^8)+b4
        if b1 >= 2^7 then
            return -(2^32-value)
        else 
            return value
        end
    end
end

function rint64()
    local b1 = string.byte(data, pos)
    local sum = b1
    for i=2, 8 do
        sum = sum + string.byte(data, pos+i-1) * math.pow(2, (8-i)*8)
    end
    pos = pos + 8 
    if b1 >= 2^7 then
        return -(2^64 - sum)
    else
        return sum
    end
end

function rbool()
    local b1 = string.byte(data, pos)
    if b1 == 0 then
        return false
    else
        return true
    end
end

function rstring()
    local len = read2byte()
    if len == 0 then
        return ""
    end
    local str = string.sub(data, pos, pos+len-1)
    pos = pos + len 
    return str 
end

function rtuple(tupleName) 
    local tag = readbyte()
    if tag == 0 then
        return false
    end
    local tuple = {class=tupleName}
    pb_unpack[tupleName](tuple)
    return tuple
end

function rlist(tp)
    local len = read2byte()
    local list = {}
    local fun
    if tp == "int32" then
        fun = rint32
    elseif tp == "int64" then
        fun = rint64
    elseif tp == "bool" then
        fun = rbool
    elseif tp == "string" then
        fun = rstring
    else
        fun = rtuple
    end
    for i=1, len do 
        table.insert(list, fun(tp))
    end

    return list
end

function wint32(int)
    if not int then
        return string.rep(string.char(0), 4)
    end

    assert(int < 2^31 and int >= -2^31)
    if math.abs(int) ~= int then
        int = 2 ^ 32 + int 
    end
    local tmp = ""
    for i=1, 4 do 
        tmp = string.char(int % base) .. tmp
        int = math.floor(int / base)
    end
    return tmp
end

function wint64(int)
    if not int then
        return string.rep(string.char(0), 8)
    end 

    assert(int < 2^63 and int >= -2^63)
    if math.abs(int) ~= int then
        int = 2^64 + int 
    end
    local tmp = ""
    for i=1, 8 do 
        tmp = string.char(int % base) .. tmp
        int = math.floor(int/base)
    end
    return tmp
end

function wbool(bool)
    b = b and 1 or 0
    return string.char(b)
end

function wstring(str)
    if type(str) ~= "string" then
        str = ""
    end
    local len = #str 
    return (write2byte(len) .. str)
end

function wtuple(tuple)
    if not tuple then
        return writeByte(0)
    end
    local str = pb_pack[tuple.class](tuple)
    return writebyte(1) .. str
end

function wlist(list, tp)
    if not list then
        list = {}
    end
    local len = #list
    local str = write2byte(len)
    local fun 
    if tp == "int32" then
        fun = wint32
    elseif tp == "int64" then
        fun = wint64
    elseif tp == "bool" then
        fun = wbool
    elseif tp == "string" then
        fun = wstring
    else 
        fun = wtuple
    end
    for i=1, len do
        str = str .. fun(list[i], tp)
    end
    return str
end









