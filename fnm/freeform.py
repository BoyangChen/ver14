inputf=open('toolkit_module.f','r')
outputf=open('toolkit_module.f90','w')

lines=inputf.readlines()
for line in lines:
    newline = line
    list1=list(line)
    if list1[0]=='c' or list1[0]=='C':
        list1[0]='!'
        newline=''.join(list1)
    outputf.write(newline)

inputf.close()
outputf.close()
