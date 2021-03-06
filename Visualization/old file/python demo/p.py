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
program = "Visualization\old file\python demo\program.txt"
helpingfunc = "Visualization\old file\python demo\helping.txt"


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
    file = open("a.txt", "w", encoding="utf-8")
    file.write(content)
    file.close()
    file_add.close()

# 替换


def alter(file, old_str, new_str):

    with open(file, "r", encoding="utf-8") as f1, open("%s.bak" % file, "w", encoding="utf-8") as f2:
        for line in f1:
            f2.write(re.sub(old_str, new_str, line))
    os.remove(file)
    os.rename("%s.bak" % file, file)


# --------------------------------------------------------------------------------


def toJsLiteral(outputadds):
    r = open(outputadds, "r")
    lines = r.readlines()
    w = open(outputadds, "w")
    for l in lines:
        l = l.replace('\n', '\\n')
        w.write(l)


# fileadds = program

# r = open(program, "r")
# lines = r.readlines()
# w = open(program, "w")
# for l in lines:
#     l = l.replace('\n', '\\n')
#     w.write(l)


# t = '\\'
# alter(program, "\n", t)

# print('\\')

file = open(helpingfunc, "r", encoding="utf-8")
lines = file.readlines()
for l in lines:
    res = l.find(";")
    print(l[:res])
