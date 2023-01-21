import random
import sys

if len(sys.argv) < 2:
    print(f"Usage {sys.argv[0]} filename #samples")
    exit()
filename = sys.argv[1]
n = int(sys.argv[2])

random.seed()
with open(filename,'r') as f:
    datelist = f.readlines()

dates = random.sample(datelist,n)
for i in range(n):
    print(dates[i].rstrip("\n"))
