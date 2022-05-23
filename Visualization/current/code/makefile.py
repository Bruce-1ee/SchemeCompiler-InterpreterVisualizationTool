import re,os

# 替换所有延续，并为延续追加一个参数以适合biwascheme执行

def alter(file,old_str,new_str):

    with open(file, "r", encoding="utf-8") as f1,open("%s.bak" % file, "w", encoding="utf-8") as f2:
        for line in f1:
            f2.write(re.sub(old_str,new_str,line))
    os.remove(file)
    os.rename("%s.bak" % file, file)



f = open("Visualization/current/code/file1","a")
f.write()



alter("./Visualization/current/code/file1", "\(exec-k\)", "(exec-k 'ok)")
alter("./Visualization/current/code/file1", "\(vm-k\)", "(vm-k 'ok)")
alter("./Visualization/current/code/file1", "\(resume-meta\)", "(resume-meta 'ok)")
alter("./Visualization/current/code/file1", "\(beginning\)", "(beginning 'ok)")
alter("./Visualization/current/code/file1", ";\(sub-frame-counter\)", "(sub-frame-counter)")
alter("./Visualization/current/code/file1", ";\(add-frame-counter\)", "(add-frame-counter)")
alter("./Visualization/current/code/file1", ";\(send-stack stack s\)", "(send-stack stack s)")






