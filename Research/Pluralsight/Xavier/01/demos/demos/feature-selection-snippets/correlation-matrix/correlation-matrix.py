import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


def azureml_main(dataframe1 = None, dataframe2 = None):

    plt.matshow(dataframe1.corr())
    plt.xticks(range(len(dataframe1.columns)), dataframe1.columns, rotation='vertical')
    plt.yticks(range(len(dataframe1.columns)), dataframe1.columns)
    plt.colorbar()
    plt.savefig("correlation.png")
    
    # Return value must be of a sequence of pandas.DataFrame
    return dataframe1,
