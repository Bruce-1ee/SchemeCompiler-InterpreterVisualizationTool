import re
import os
import shutil


# """
# 1 删除位于头部的#lang等DrRacket中的语言定义等内容 'delUselessLines'
# 2
#       1 替换所有无参数延续的调用为有参数延续调用,以配合biwascheme使用
#       2 将用于biwascheme的函数的注释取消
# 3 将helping函数粘贴至文件的头部
# """

# ↓↓↓↓↓↓文件地址定义的变量在这里↓↓↓↓↓↓
inputpath = 'Compiler&interperter/meta-system.scm'
helpingfunc = 'Compiler&interperter/helping-function-js.scm'
outputpath = 'Visualization/current/code/meta-js/meta-js.scm'
jsfile = 'Visualization/current/code//test.js'

# 删除头部4行


def delUselessLines(fileadds):
    r = open(fileadds, "r", encoding="utf-8")
    # r = open("Visualization/old file/python demo/a.txt", "r")
    lines = r.readlines()
    c = 0
    w = open(fileadds, "w", encoding="utf-8")
    # w = open("Visualization/old file/python demo/a.txt", "w")
    for l in lines:
        if c > 4:
            w.write(l)
        c += 1

# 将文件hepfunc中的内容拷贝至fileadds文件的头部


def addHelpFunction(fileadds, hepfunc):

    file = open(fileadds, "r", encoding="utf-8")
    file_add = open(hepfunc, "r", encoding="utf-8")
    # file = open("Visualization/old file/python demo/a.txt", "r")
    # file_add = open("Visualization/old file/python demo/b.txt", "r")
    content = file.read()
    content_add = file_add.read()
    # pos = content.find("begin")
    pos = 0

    content = content[:pos] + content_add + "\n" + content[pos:]
    file = open(fileadds,
                "w", encoding="utf-8")
    file.write(content)
    file.close()
    file_add.close()


def toJsLiteral(outputadds):

    r = open(outputadds, "r", encoding="utf-8")
    lines = r.readlines()
    w = open(outputadds, "w", encoding="utf-8")
    for l in lines:
      #   pos = l.find(";")
      #   l = l[:pos]
      #   l = l.replace('\n', '\\n')
        l = l.replace('\n', ' ')
        l = l.replace("'", "\\'")
        w.write(l)
    file = open(outputadds, "r", encoding="utf-8")
    content = file.read()
    content = "meta = '" + content
    file = open(outputadds, "w", encoding="utf-8")
    file.write(content)
    file.close()


def addtail(outputadds):
    w = open(outputadds, "a", encoding="utf-8")
    w.write("'")


# 替换


def alter(file, old_str, new_str):

    with open(file, "r", encoding="utf-8") as f1, open("%s.bak" % file, "w", encoding="utf-8") as f2:
        for line in f1:
            f2.write(re.sub(old_str, new_str, line))
    os.remove(file)
    os.rename("%s.bak" % file, file)


# --------------------------------------------------------------------------------


shutil.copyfile(inputpath, outputpath)

delUselessLines(outputpath)

alter(outputpath, "\(exec-k\)", "(exec-k 'ok)")
alter(outputpath, "\(vm-k\)", "(vm-k 'ok)")
alter(outputpath, "\(resume-meta\)", "(resume-meta 'ok)")
alter(outputpath, "\(beginning\)", "(beginning 'ok)")
#alter(outputpath, "\(break-meta\)", "(break-meta 'ok)")

alter(outputpath, "display", "console-log")

alter(outputpath, "\(next3\)", "(next3 'ok)")


addHelpFunction(outputpath, helpingfunc)

# shutil.copyfile(outputpath, jsfile)

# toJsLiteral(jsfile)
# addtail(jsfile)
