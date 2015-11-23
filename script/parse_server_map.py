
import os
import json
import string
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
    def __init__(self, targets=["erl", "lua"], basedir=None):
        # print(targets)
        basedir = basedir or parse_util.proto_dir()
        f = os.path.join(basedir, "server_map.JSON")
        f = open(f)
        f = f.read()
        self.mapdata = json.loads(f)
        self.targets = [get_instance(target) for target in targets]

    def parse(self):
        self.create_file()
        for data in self.mapdata:
            self.parse_map_module(data)
        self.close_file()

    def create_file(self):
        for target in self.targets:
            target.create_file()

    def close_file(self):
        for target in self.targets:
            target.close_file()

    def parse_map_module(self, data):
        modname = data["name"]
        modval = data["value"]
        methods = data["methods"]
        for target in self.targets:
            target.generate_pb_map(modname, modval, methods)

class erl(object):
    def __init__(self):
        pass
    def __del__(self):
        if self.pb_map:
            self.pb_map.close()
        if self.mm_define:
            self.mm_define.close()

    def create_file(self):
        dir = parse_util.target_dir("erl")
        parse_util.ensure_dir(dir)
        pb_map_name = os.path.join(dir, "pb_map.erl")
        mm_define_name = os.path.join(dir, "mm_define.hrl")

        self.pb_map = open(pb_map_name, "wb")
        self.mm_define = open(mm_define_name, "wb")
        self.tmpArr1 = []
        self.tmpArr2 = []

        ##write headers
        self.write_header()
    def write_header(self):
        head = '''-module(pb_map).
-author("wanghaohao").
%% API
-export([m_to_f/1,
         f_to_m/1]).

'''
        self.pb_map.write(head)

    def close_file(self):
        self.pb_map.write("".join(self.tmpArr1))
        self.pb_map.write("m_to_f(_) ->  undefined.\n\n")
        self.pb_map.write("".join(self.tmpArr2))
        self.pb_map.write("f_to_m(_) ->  0.\n")

        self.pb_map.close()
        self.mm_define.close()
        self.pb_map = None
        self.mm_define = None

    def generate_pb_map(self, modname, modval, methods):
        self.generate_pb_map_file(modname, modval, methods)
        self.generate_mm_define_file(modname, modval, methods)

    def generate_pb_map_file(self, _modname, _modval, methods):
        pass

    def generate_mm_define_file(self, modname, modval, methods):
        self.mm_define.write("-define(%s, %s).\n" % (string.upper(modname), modval))
        for method in methods:
            for key in method:
                upkey = string.upper(key)
                lowerkey = string.lower(key)
                id = method[key]

                self.mm_define.write("-define(%s, %s).\n" % (upkey, id))
                self.tmpArr1.append("m_to_f(%s) ->   m_%s_tos;\n" % (id, lowerkey))
                self.tmpArr2.append("f_to_m(m_%s_toc) ->  %s;\n" % (lowerkey, id))

class lua(object):
    def create_file(self):
        dir = parse_util.target_dir("lua")
        parse_util.ensure_dir(dir)
        pb_map_name = os.path.join(dir, "pb_map.lua")
        self.pb_map = open(pb_map_name, "wb")
        self.write_header()

        self.tmpArr1 = []
        self.tmpArr2 = []

    def write_header(self):
        head = '''
module("pb_map", package.seeall)

'''
        self.pb_map.write(head)

    def generate_pb_map(self, _modname, _modval, methods):
        for method in methods:
            for key in method:
                lowerkey = string.lower(key)
                id = method[key]

                self.tmpArr1.append("[%s]=\"m_%s_toc\",\n" % (id, lowerkey))
                self.tmpArr2.append("m_%s_tos=%s,\n" % (lowerkey, id))
        pass

    def __del__(self):
        if self.pb_map:
            self.pb_map.close()

    def close_file(self):
        self.pb_map.write("m2f={\n")
        self.pb_map.write("".join(self.tmpArr1))
        self.pb_map.write("}\n")
        self.pb_map.write("f2m={\n")
        self.pb_map.write("".join(self.tmpArr2))
        self.pb_map.write("}")
        self.pb_map.close()
        self.pb_map = None


