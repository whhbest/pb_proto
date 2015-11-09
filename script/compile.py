#!/usr/bin/env python
# import sys
# import os

# import parse_util
import parse_server_map
import parse_proto

def run():
    # path = sys.argv[0]
    # print os.path.dirname(os.path.abspath(path))
    # print parse_util.proto_dir()
    mapParse = parse_server_map.parser(["erl", "lua"])
    mapParse.parse()
    protoParse =parse_proto.parser(["erl", "lua"])
    protoParse.parse()

if __name__ == '__main__':
    run()