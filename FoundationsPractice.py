from math import sqrt

#Task T01: Counting

#for i in range(0,11):
    #print(i)


for i in range (1,11):
    while(i<=10):
        print(i) #print(i," ", end="")
        i+=10

#Task T02: Nested Loops

#for week in range (1,53):
    #for day in ["mon", "tue", "wed", "thur", "fri", "sat", "sun"]:
        #print(week, day)

for i in reversed(range (1,9)):
    for j in range (1,9):
        print("(",i,",",j,")","",end="")
    print("(",i,",",j,")","")

#Task T03: Calculations With Nested Loops

for i in range (1,9):
    for j in range (1,9):
        for ii in range (1,9):
            for jj in range (1,9):
                if ii==i&jj==j:
                    continue
                distance=sqrt((ii-i)**2+(jj-j)**2)
                print("(",i,",",j,")","to","(",ii,",",jj,")","distance",distance)

