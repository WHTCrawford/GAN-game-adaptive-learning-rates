import os
import numpy as np
import pandas as pd
os.chdir('/Users/Billy/PycharmProjects/GALR/data/testing/')


real_dist = np.random.normal(5, 1, (3,2002))
res_dataframe = pd.DataFrame(data=real_dist.astype(float))

with open("output.csv", 'a') as f:
    res_dataframe.to_csv(f, header=False,index=False)