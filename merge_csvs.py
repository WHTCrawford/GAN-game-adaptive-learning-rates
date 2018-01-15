import os
os.chdir('/Users/Billy/PycharmProjects/GALR/data/gd/')
start_file_number = 2
end_file_number = 438


fout=open("output.csv","w")
for num in range(start_file_number,end_file_number+1):
    f = open("output"+str(num)+".csv")
    for line in f:
         fout.write(line)
    os.remove("output"+str(num)+".csv")
fout.close()