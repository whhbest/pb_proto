import sys
import os
import re
import string

def base_dir():
    path = sys.argv[0]
    path = os.path.dirname(os.path.abspath(path))
    (base, _) = os.path.split(path)
    return  base

def proto_dir():
    baseDir = base_dir()
    return os.path.join(baseDir, "proto")

def script_dir():
    baseDir = base_dir()
    return os.path.join(baseDir, "script")

def target_dir(target):
    baseDir = base_dir()
    return os.path.join(baseDir, "target", target)

def is_comment(str):
    return re.match("^\s*(\/+|%+)", str)

def is_blank_line(str):
    return len(string.strip(str)) == 0

def is_message_begin(str):
    return re.match("^\s*message", str)

def is_message_end(str):
    return re.match("^\s*end", str)

def ensure_dir(dir):
    if not os.path.exists(dir):
        os.makedirs(dir)

def get_all_proto():
    files = []
    protoDir = proto_dir()
    for f in os.listdir(protoDir):
        # print(f)
        if re.search("\.proto$", f):
            files.append(os.path.join(protoDir, f))

    return files

class myerror(BaseException):
    def __init__(self, line, msg):
        self.line = line
        self.msg = msg

    def __str__(self):
        return "[line: %s] %s" % (self.line, self.msg)

class helper(object):
    def __init__(self, handler):
        self.curline = 0
        self.curfile = ""
        self.handler = handler
        self.initTmpdata()

    def initTmpdata(self):
        self.isBegin = False
        self.isEnd = False
        self.tmpDict = {"head":False, "body": []}

    def parse_head(self, str):
        s = string.split(str, " ")
        return string.rstrip(s[1], "\n\0")

    def parse_body(self, str):
        arr = string.split(str, ";")
        if not arr[0]:
            raise myerror(self.curline, "message define error")
        arr = string.split(string.strip(arr[0]))
        arrlen = len(arr)
        if arrlen >3 or arrlen < 2:
            raise myerror(self.curline, "message define error")
        if arrlen == 3 and arr[1] != "*":
            raise myerror(self.curline, "message define error")

        body = {}
        if arrlen == 3:
            body["type"] = arr[0]
            body["isarray"] = True
            body["name"] = arr[2]
        elif arrlen == 2:
            body["name"] = arr[1]
            if arr[0][-1] == "*":
                body["isarray"] = True
            else:
                body["isarray"] = False

            body["type"] = string.rstrip(arr[0], "*")

        return body

    def set_line(self, line):
        self.curline = self.curline + 1
        line = string.strip(line, " \0")
        if not is_comment(line) and not is_blank_line(line):
            self.isBegin = is_message_begin(line)
            self.isEnd = is_message_end(line)
            if self.isBegin:
                head = self.parse_head(line)
                if self.tmpDict["head"] or not head:
                    raise myerror(self.curline, "invalid message head")
                self.tmpDict["head"] = head
            elif self.isEnd:
                if not self.tmpDict["head"]:
                    raise myerror(self.curline, "message begin and end no match")
                ## get a message
                self.handler(self.tmpDict)
                self.initTmpdata()
            else:
                ## body define
                if not self.tmpDict["head"]:
                    raise myerror(self.curline, "miss message begin")

                body = self.parse_body(line)
                self.tmpDict["body"].append(body)




