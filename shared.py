import pandas as pd

def load_data():
    return pd.read_csv('data/2025-09-30 Chris data/data/micropantries_oo.csv', nrows = None, on_bad_lines= 'skip')

a = load_data()
