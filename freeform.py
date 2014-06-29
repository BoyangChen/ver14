inputf=open('UEL2d-90a.f','r')
outputf=open('uel2d-90free.f','w')

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
