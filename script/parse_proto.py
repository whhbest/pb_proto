
import os
import string
import re
import parse_util

def get_instance(target):
    # print(target)
    cls = False
    if target == 'erl':
        cls = erl
    elif target == 'lua':
        cls = lua

    return  cls()

class parser(object):
    def __init__(self, targets=["erl", "lua"]):
        self.proto_files = parse_util.get_all_proto()
        self.targets = [get_instance(target) for target in targets]

    def parse(self):
        self.create_file()
        for proto in self.proto_files:
            f = open(proto,"r")
            helper = parse_util.helper(self.parse_handler)
            while True:
                line = f.readline()
                if line:
                    helper.set_line(line)
                else:
                    break
            f.close()

        self.close_file()

    def create_file(self):
        for target in self.targets:
            target.create_file()

    def close_file(self):
        for target in self.targets:
            target.close_file()

    def parse_handler(self, msg):
        # print msg
        for target in self.targets:
            target.generate_proto_file(msg)


class erl(object):
    def create_file(self):
        dir = parse_util.target_dir("erl")
        pb_pack_name = os.path.join(dir, "pb_pack.erl")
        pb_unpack_name = os.path.join(dir, "pb_unpack.erl")
        all_pb_name = os.path.join(dir, "all_pb.hrl")

        self.pb_pack = open(pb_pack_name, "wb")
        self.pb_unpack = open(pb_unpack_name, "wb")
        self.all_pb = open(all_pb_name, "wb")
        self.write_header()

    def close_file(self):
        self.pb_pack.close()
        self.pb_unpack.close()
        self.all_pb.close()

    def write_header(self):
        header='''-module(pb_pack).
-author("wanghaohao").
-compile(export_all).

-define(I32(V), (pb:wint32(V))/binary).
-define(I64(V), (pb:wint64(V))/binary).
-define(B(V), (pb:wbool(V))/binary).
-define(S(V), (pb:wstring(V))/binary).
-define(T(V, Type), (pb:wtuple(V, Type))/binary).
-define(L(V, Type), (pb:wlist(V, Type))/binary).

'''
        self.pb_pack.write(header)

        header='''-module(pb_unpack).
-author("wanghaohao").
-compile(export_all).

-define(I32(V), pb:rint32(V)).
-define(I64(V), pb:rint64(V)).
-define(B(V), pb:rbool(V)).
-define(S(V), pb:rstring(V)).
-define(T(V, Type), pb:rtuple(V, Type)).
-define(L(V, Type), pb:rlist(V, Type)).


'''
        self.pb_unpack.write(header)


    def generate_proto_file(self, msg):
        self.generate_all_pb_file(msg)
        if re.search("tos$", msg["head"]):
            self.generate_pb_unpack_file(msg)
        elif re.search("toc$", msg["head"]):
            self.generate_pb_pack_file(msg)
        else:
            self.generate_pb_unpack_file(msg)
            self.generate_pb_pack_file(msg)

    def generate_all_pb_file(self, msg):
        names = []
        for body in msg["body"]:
            names.append(body["name"])
        self.all_pb.write("-record(%s, {%s}).\n" % (msg["head"], string.join(names, ",")))

    def generate_pb_unpack_file(self, msg):
        self.pb_unpack.write("%s(B0) -> \n" % msg["head"])
        tmpArr = [msg["head"]]
        i = 0
        for body in msg["body"]:
            i = i+1
            if body["isarray"]:
                self.pb_unpack.write("   {V%s, B%s}=?L(B%s, %s),\n" % (i, i, i-1, body["type"]))
            else:
                if body["type"] == "int32":
                    self.pb_unpack.write("   {V%s, B%s}=?I32(B%s),\n" % (i, i, i-1))
                elif body["type"] == "int64":
                    self.pb_unpack.write("   {V%s, B%s}=?64(B%s),\n" % (i,i,i-1))
                elif body["type"] == "bool":
                    self.pb_unpack.write("   {v%s, B%s}=?B(B%s),\n" % (i,i,i-1))
                elif body["type"] == "string":
                    self.pb_unpack.write("   {v%s, B%s}=?S(B%s),\n" % (i,i,i-1))
                else:
                    self.pb_unpack.write("   {v%s, B%s}=?T(B%s, %s),\n" % (i,i,i-1,body["type"]))
            tmpArr.append("V%s" % i)

        self.pb_unpack.write("   {{%s}, B%s}.\n" % (string.join(tmpArr,","), i))

    def generate_pb_pack_file(self, msg):
        tmpArr = ["_"]
        i = 1
        for _ in range(len(msg["body"])):
            tmpArr.append("V%s" % i)
            i = i+1

        self.pb_pack.write("%s(%s) -> \n" % (msg["head"], string.join(tmpArr, ",")))
        tmpArr = []
        i = 1
        for body in msg["body"]:
            if body["isarray"]:
                tmpArr.append("?L(V%s, %s)" % (i, body["type"]))
            else:
                if body["type"] == "int32":
                    tmpArr.append("?I32(V%s)" % i)
                elif body["type"] == "int64":
                    tmpArr.append("?I64(V%s)" % i)
                elif body["type"] == "bool":
                    tmpArr.append("?B(V%s)" % i)
                elif body["type"] == "string":
                    tmpArr.append("?S(V%s)" % i)
                else:
                    tmpArr.append("?T(V%s, %s)" % (i, body["type"]))

            i = i+1

        self.pb_pack.write("   <<%s>>\n" % string.join(tmpArr, ","))


class lua(object):
    def create_file(self):
        dir = parse_util.target_dir("lua")
        pb_pack_name = os.path.join(dir, "pb_pack.lua")
        pb_unpack_name = os.path.join(dir, "pb_unpack.lua")

        self.pb_pack = open(pb_pack_name, "wb")
        self.pb_unpack = open(pb_unpack_name, "wb")
        self.write_header()

    def close_file(self):
        self.pb_pack.close()
        self.pb_unpack.close()

    def generate_proto_file(self, msg):
        if re.search("toc$", msg["head"]):
            self.generate_pb_unpack_file(msg)
        elif re.search("tos$", msg["head"]):
            self.generate_pb_pack_file(msg)
        else:
            self.generate_pb_unpack_file(msg)
            self.generate_pb_pack_file(msg)

    def generate_pb_pack_file(self, msg):
        self.pb_pack.write("function %s(t)\n" % msg["head"])
        for body in msg["body"]:
            if body["isarray"]:
                self.pb_pack.write("   s=s..L(t.%s, \"%s\")\n" % (body["name"], body["type"]))
            else:
                if body["type"] == "int32":
                    self.pb_pack.write("  s=s..I32(t.%s)\n" % body["name"])
                elif body["type"] == "int64":
                    self.pb_pack.write("   s=s..I64(t.%s)\n" % body["name"])
                elif body["type"] == "bool":
                    self.pb_pack.write("   s=s..B(t.%s)\n" % body["name"])
                elif body["type"] == "string":
                    self.pb_pack.write("   s=s..S(t.%s)\n" % body["name"])
                else:
                    self.pb_pack.write("   s=s..T(t.%s, \"%s\")\n" % body["name"])

        self.pb_pack.write("   return s\nend\n")

    def generate_pb_unpack_file(self, msg):
        self.pb_unpack.write("function %s(m)\n" % msg["head"])
        for body in msg["body"]:
            if body["isarray"]:
                self.pb_unpack.write("   m.%s=L(\"%s\")\n" % (body["name"], body["type"]))
            else:
                if body["type"] == "int32":
                    self.pb_unpack.write("  m.%s=I32()\n" % body["name"])
                elif body["type"] == "int64":
                    self.pb_unpack.write("   m.%s=I64()\n" % body["name"])
                elif body["type"] == "bool":
                    self.pb_unpack.write("   m.%s=B()\n" % body["name"])
                elif body["type"] == "sting":
                    self.pb_unpack.write("   m.%s=S()\n" % body["name"])
                else:
                    self.pb_unpack.write("   m.%s=T(%s)\n" % (body["name"], body["type"]))
        self.pb_unpack.write("end\n")



    def write_header(self):
        header='''module("pb_pack", package.seeall)

local I32 = pb.wint32
local I64 = pb.wint64
local B = pb.wbool
local S = pb.wstring
local T = pb.wtuple
local L = pb.wlist

'''
        self.pb_pack.write(header)

        header='''module("pb_unpack", package.seeall)
local I32 = pb.rint32
local I64 = pb.rint64
local B = pb.rbool
local S = pb.rstring
local T = pb.rtuple
local L = pb.rlist

'''
        self.pb_unpack.write(header)

