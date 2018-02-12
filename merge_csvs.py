import os
os.chdir('/Users/Billy/PycharmProjects/GALR/data4/gd/')
start_file_number = 1
end_file_number = 2


fout=open("output_merged.csv","w")
for num in range(start_file_number,end_file_number+1):
    f = open("output"+str(num)+".csv")
    for line in f:
         fout.write(line)
    os.remove("output"+str(num)+".csv")
fout.close()