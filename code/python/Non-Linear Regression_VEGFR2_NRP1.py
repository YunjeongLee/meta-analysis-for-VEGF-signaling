import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import os
import pandas as pd

# Change the working directory 
print(os.getcwd()) # Prints the current working directory
# Provide the new path here
os.chdir('C:/Users/lione/Desktop/GitHub/meta-analysis-for-VEGF-signaling/data') 
# Prints the new working directory
print(os.getcwd())

# Import data & clean it

def import_and_clean_data(file_name, sheet_name, columns):
    data = pd.read_excel(file_name, sheet_name=sheet_name)
    cleaned_data = data.loc[:, columns].dropna()
    return cleaned_data

vegfa165_vegfr2l05 = import_and_clean_data("VEGFA165_VEGFR2_0.5-8nMLu2023.xlsx", "Sheet1", ["Time 0.5nM", "RU 0.5nM"])
vegfa165_vegfr2l1 = import_and_clean_data("VEGFA165_VEGFR2_0.5-8nMLu2023.xlsx", "Sheet1", ["Time 1nM", "RU 1nM"])
vegfa165_vegfr2l2 = import_and_clean_data("VEGFA165_VEGFR2_0.5-8nMLu2023.xlsx", "Sheet1", ["Time 2nM", "RU 2nM"])
vegfa165_vegfr2l4 = import_and_clean_data("VEGFA165_VEGFR2_0.5-8nMLu2023.xlsx", "Sheet1", ["Time 4nM", "RU 4nM"])
vegfa165_vegfr2l8 = import_and_clean_data("VEGFA165_VEGFR2_0.5-8nMLu2023.xlsx", "Sheet1", ["Time 8nM", "RU 8nM"])

vegfa165_nrp1l05 = import_and_clean_data("VEGFA165_NRP1_0.5-8nM_Lu2023.xlsx", "Sheet1", ["Time 0.5nM", "RU 0.5nM"])
vegfa165_nrp1l1 = import_and_clean_data("VEGFA165_NRP1_0.5-8nM_Lu2023.xlsx", "Sheet1", ["Time 1nM", "RU 1nM"])
vegfa165_nrp1l2 = import_and_clean_data("VEGFA165_NRP1_0.5-8nM_Lu2023.xlsx", "Sheet1", ["Time 2nM", "RU 2nM"])
vegfa165_nrp1l4 = import_and_clean_data("VEGFA165_NRP1_0.5-8nM_Lu2023.xlsx", "Sheet1", ["Time 4nM", "RU 4nM"])
vegfa165_nrp1l8 = import_and_clean_data("VEGFA165_NRP1_0.5-8nM_Lu2023.xlsx", "Sheet1", ["Time 8nM", "RU 8nM"])

vegfa165_nrp1h23 = import_and_clean_data("VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", "Sheet1", ["Time 2.3nM","RU 2.3nM"])
vegfa165_nrp1h46 = import_and_clean_data("VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", "Sheet1", ["Time 4.6nM","RU 4.6nM"])
vegfa165_nrp1h92 = import_and_clean_data("VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", "Sheet1", ["Time 9.2nM","RU 9.2nM"])
vegfa165_nrp1h19 = import_and_clean_data("VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", "Sheet1", ["Time 19.5nM","RU 19.5nM"])
vegfa165_nrp1h39 = import_and_clean_data("VEGFA165_NRP1_02.3-39nm_Herve2008.xlsx", "Sheet1", ["Time 39nM","RU 39nM"])

# Split the data int rise and decay

def split_data(data, time_col, ru_col):
    max_index = data[ru_col].idxmax()
    mask = (data[time_col] >= 0) & (data.index <= max_index)
    rise = data.loc[mask]
    decay = data.loc[max_index+1:]
    return rise, decay

datasets = {
    'vegfa165_vegfr2l05': ['Time 0.5nM', 'RU 0.5nM'],
    'vegfa165_vegfr2l1': ['Time 1nM', 'RU 1nM'],
    'vegfa165_vegfr2l2': ['Time 2nM', 'RU 2nM'],
    'vegfa165_vegfr2l4': ['Time 4nM', 'RU 4nM'],
    'vegfa165_vegfr2l8': ['Time 8nM', 'RU 8nM'],
    'vegfa165_nrp1l05': ['Time 0.5nM', 'RU 0.5nM'],
    'vegfa165_nrp1l1': ['Time 1nM', 'RU 1nM'],
    'vegfa165_nrp1l2': ['Time 2nM', 'RU 2nM'],
    'vegfa165_nrp1l4': ['Time 4nM', 'RU 4nM'],
    'vegfa165_nrp1l8': ['Time 8nM', 'RU 8nM'],
    'vegfa165_nrp1h23': ['Time 2.3nM', 'RU 2.3nM'],
    'vegfa165_nrp1h46': ['Time 4.6nM', 'RU 4.6nM'],
    'vegfa165_nrp1h92': ['Time 9.2nM', 'RU 9.2nM'],
    'vegfa165_nrp1h19': ['Time 19.5nM', 'RU 19.5nM'],
    'vegfa165_nrp1h39': ['Time 39nM', 'RU 39nM']
}

for dataset_name, cols in datasets.items():
    time_col, ru_col = cols
    dataset = globals()[dataset_name]
    rise, decay = split_data(dataset, time_col, ru_col)
    globals()[f'{dataset_name}_rise'] = rise
    globals()[f'{dataset_name}_decay'] = decay

# Define the function to fit the data

def rt_ka_function(rmax,conc,KD,ka,kd,t):
    return ((rmax*conc) / (KD + conc) ) * ( 1 - 1/(np.exp(ka*conc + kd)*t))

def rt_kd_function(r0,kd,t):
    return (r0*np.exp(-kd*t))

# Fit the data

def fit_data(data, time_col, ru_col, function):
    param_ka, pcov_ka = curve_fit(function, data[time_col], data[ru_col])
    return param_ka, pcov_ka

datasets = {
    'vegfa165_vegfr2l05_rise': ['Time 0.5nM', 'RU 0.5nM', rt_ka_function],
    'vegfa165_vegfr2l05_decay': ['Time 0.5nM', 'RU 0.5nM', rt_kd_function],
    'vegfa165_vegfr2l1_rise': ['Time 1nM', 'RU 1nM', rt_ka_function],
    'vegfa165_vegfr2l1_decay': ['Time 1nM', 'RU 1nM', rt_kd_function],
    'vegfa165_vegfr2l2_rise': ['Time 2nM', 'RU 2nM', rt_ka_function],
    'vegfa165_vegfr2l2_decay': ['Time 2nM', 'RU 2nM', rt_kd_function],
    'vegfa165_vegfr2l4_rise': ['Time 4nM', 'RU 4nM', rt_ka_function],
    'vegfa165_vegfr2l4_decay': ['Time 4nM', 'RU 4nM', rt_kd_function],
    'vegfa165_vegfr2l8_rise': ['Time 8nM', 'RU 8nM', rt_ka_function],
    'vegfa165_vegfr2l8_decay': ['Time 8nM', 'RU 8nM', rt_kd_function],

    'vegfa165_nrp1l05_rise': ['Time 0.5nM', 'RU 0.5nM', rt_ka_function],
    'vegfa165_nrp1l05_decay': ['Time 0.5nM', 'RU 0.5nM', rt_kd_function],
    'vegfa165_nrp1l1_rise': ['Time 1nM', 'RU 1nM', rt_ka_function],
    'vegfa165_nrp1l1_decay': ['Time 1nM', 'RU 1nM', rt_kd_function],
    'vegfa165_nrp1l2_rise': ['Time 2nM', 'RU 2nM', rt_ka_function],
    'vegfa165_nrp1l2_decay': ['Time 2nM', 'RU 2nM', rt_kd_function],
    'vegfa165_nrp1l4_rise': ['Time 4nM', 'RU 4nM', rt_ka_function],
    'vegfa165_nrp1l4_decay': ['Time 4nM', 'RU 4nM', rt_kd_function],
    'vegfa165_nrp1l8_rise': ['Time 8nM', 'RU 8nM', rt_ka_function],
    'vegfa165_nrp1l8_decay': ['Time 8nM', 'RU 8nM', rt_kd_function],

    'vegfa165_nrp1h23_rise': ['Time 2.3nM', 'RU 2.3nM', rt_ka_function],
    'vegfa165_nrp1h23_decay': ['Time 2.3nM', 'RU 2.3nM', rt_kd_function],
    'vegfa165_nrp1h46_rise': ['Time 4.6nM', 'RU 4.6nM', rt_ka_function],
    'vegfa165_nrp1h46_decay': ['Time 4.6nM', 'RU 4.6nM', rt_kd_function],
    'vegfa165_nrp1h92_rise': ['Time 9.2nM', 'RU 9.2nM', rt_ka_function],
    'vegfa165_nrp1h92_decay': ['Time 9.2nM', 'RU 9.2nM', rt_kd_function],
    'vegfa165_nrp1h19_rise': ['Time 19.5nM', 'RU 19.5nM', rt_ka_function],
    'vegfa165_nrp1h19_decay': ['Time 19.5nM', 'RU 19.5nM', rt_kd_function],
    'vegfa165_nrp1h39_rise': ['Time 39nM', 'RU 39nM', rt_ka_function],
    'vegfa165_nrp1h39_decay': ['Time 39nM', 'RU 39nM', rt_kd_function]
}

for dataset_name, cols in datasets.items():
    time_col, ru_col, function = cols
    dataset = globals()[dataset_name]
    param, pcov = fit_data(dataset, time_col, ru_col, function)
    globals()[f'param_{dataset_name}'], globals()[f'pcov_{dataset_name}'] = param, pcov


# Plot the data

# Define a function to plot the data
def plot_data(data_rise, time_col, ru_col, function, param_rise, label):
    plt.scatter(data_rise[time_col], data_rise[ru_col], label=f'{label} Data')
    plt.plot(data_rise[time_col], function(data_rise[time_col], *param_rise), 'r-', label=f'{label} Fit')

# Define the datasets and parameters
dataset_vrl = {
    'vegfa165_vegfr2l05': [vegfa165_vegfr2l05_rise, "Time 0.5nM", "RU 0.5nM", rt_ka_function, param_vegfa165_vegfr2l05_rise, '0.5nM'],
    'vegfa165_vegfr2l1': [vegfa165_vegfr2l1_rise, "Time 1nM", "RU 1nM", rt_ka_function, param_vegfa165_vegfr2l1_rise, '1nM'],
    'vegfa165_vegfr2l2': [vegfa165_vegfr2l2_rise, "Time 2nM", "RU 2nM", rt_ka_function, param_vegfa165_vegfr2l2_rise, '2nM'],
    'vegfa165_vegfr2l4': [vegfa165_vegfr2l4_rise, "Time 4nM", "RU 4nM", rt_ka_function, param_vegfa165_vegfr2l4_rise, '4nM'],
    'vegfa165_vegfr2l8': [vegfa165_vegfr2l8_rise, "Time 8nM", "RU 8nM", rt_ka_function, param_vegfa165_vegfr2l8_rise, '8nM'],
}

dataset_vnh = {
    'vegfa165_nrp1l05': [vegfa165_nrp1l05_rise, "Time 0.5nM", "RU 0.5nM", rt_ka_function, param_vegfa165_nrp1l05_rise, '0.5nM'],
    'vegfa165_nrp1l1': [vegfa165_nrp1l1_rise, "Time 1nM", "RU 1nM", rt_ka_function, param_vegfa165_nrp1l1_rise, '1nM'],
    'vegfa165_nrp1l2': [vegfa165_nrp1l2_rise, "Time 2nM", "RU 2nM", rt_ka_function, param_vegfa165_nrp1l2_rise, '2nM'],
    'vegfa165_nrp1l4': [vegfa165_nrp1l4_rise, "Time 4nM", "RU 4nM", rt_ka_function, param_vegfa165_nrp1l4_rise, '4nM'],
    'vegfa165_nrp1l8': [vegfa165_nrp1l8_rise, "Time 8nM", 'RU 8nM', rt_ka_function, param_vegfa165_nrp1l8_rise, '8nM'],
}

dataset_vrh = {
    'vegfa165_nrp1h23': [vegfa165_nrp1h23_rise, "Time 2.3nM", "RU 2.3nM", rt_ka_function, param_vegfa165_nrp1h23_rise, '2.3nM'],
    'vegfa165_nrp1h46': [vegfa165_nrp1h46_rise, "Time 4.6nM", "RU 4.6nM", rt_ka_function, param_vegfa165_nrp1h46_rise, '4.6nM'],
    'vegfa165_nrp1h92': [vegfa165_nrp1h92_rise, "Time 9.2nM", "RU 9.2nM", rt_ka_function, param_vegfa165_nrp1h92_rise, '9.2nM'],
    'vegfa165_nrp1h19': [vegfa165_nrp1h19_rise, "Time 19.5nM", "RU 19.5nM",rt_ka_function, param_vegfa165_nrp1h19_rise, '19.5nM'],
    'vegfa165_nrp1h39': [vegfa165_nrp1h39_rise, "Time 39nM", "RU 39nM", rt_ka_function, param_vegfa165_nrp1h39_rise, '39nM']    
}

# Plot the data for each dataset

plt.figure()
for dataset_name, params in dataset_vrl.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:VEGFR2 Lu2023 Data')
plt.show()

plt.figure()
for dataset_name, params in dataset_vnh.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Lu2023 Data')
plt.show()

plt.figure()
for dataset_name, params in dataset_vrh.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Herve2008 Data')
plt.show()

# Generate a dataframe with the parameters for each dataset

#VEGFA165:VEGFR2 Lu2023 Data
vegfa_vegf2_lu2023_results = pd.DataFrame({
'0.5nM': [param_vegfa165_vegfr2l05_rise[0], param_vegfa165_vegfr2l05_rise[1], param_vegfa165_vegfr2l05_rise[2], param_vegfa165_vegfr2l05_rise[3], param_vegfa165_vegfr2l05_rise[4]],
'1nM': [param_vegfa165_vegfr2l1_rise[0], param_vegfa165_vegfr2l1_rise[1], param_vegfa165_vegfr2l1_rise[2], param_vegfa165_vegfr2l1_rise[3], param_vegfa165_vegfr2l1_rise[4]],
'2nM': [param_vegfa165_vegfr2l2_rise[0], param_vegfa165_vegfr2l2_rise[1], param_vegfa165_vegfr2l2_rise[2], param_vegfa165_vegfr2l2_rise[3], param_vegfa165_vegfr2l2_rise[4]],
'4nM': [param_vegfa165_vegfr2l4_rise[0], param_vegfa165_vegfr2l4_rise[1], param_vegfa165_vegfr2l4_rise[2], param_vegfa165_vegfr2l4_rise[3], param_vegfa165_vegfr2l4_rise[4]],
'8nM': [param_vegfa165_vegfr2l8_rise[0], param_vegfa165_vegfr2l8_rise[1], param_vegfa165_vegfr2l8_rise[2], param_vegfa165_vegfr2l8_rise[3], param_vegfa165_vegfr2l8_rise[4]]})
vegfa_vegf2_lu2023_results.index=['rmax', 'conc', 'KD', 'ka', 'kd']
print("VEGFA165:VEGFR2 Lu2023 Data")
print(vegfa_vegf2_lu2023_results)

#VEGFA165:NRP1 Lu2023 Data
vegfa_nrp1_lu2023_results = pd.DataFrame({
'0.5nM': [param_vegfa165_nrp1l05_rise[0], param_vegfa165_nrp1l05_rise[1], param_vegfa165_nrp1l05_rise[2], param_vegfa165_nrp1l05_rise[3], param_vegfa165_nrp1l05_rise[4]],
'1nM': [param_vegfa165_nrp1l1_rise[0], param_vegfa165_nrp1l1_rise[1], param_vegfa165_nrp1l1_rise[2], param_vegfa165_nrp1l1_rise[3], param_vegfa165_nrp1l1_rise[4]],
'2nM': [param_vegfa165_nrp1l2_rise[0], param_vegfa165_nrp1l2_rise[1], param_vegfa165_nrp1l2_rise[2], param_vegfa165_nrp1l2_rise[3], param_vegfa165_nrp1l2_rise[4]],
'4nM': [param_vegfa165_nrp1l4_rise[0], param_vegfa165_nrp1l4_rise[1], param_vegfa165_nrp1l4_rise[2], param_vegfa165_nrp1l4_rise[3], param_vegfa165_nrp1l4_rise[4]],
'8nM': [param_vegfa165_nrp1l8_rise[0], param_vegfa165_nrp1l8_rise[1], param_vegfa165_nrp1l8_rise[2], param_vegfa165_nrp1l8_rise[3], param_vegfa165_nrp1l8_rise[4]]})
vegfa_nrp1_lu2023_results.index=['rmax', 'conc', 'KD', 'ka', 'kd']
print("VEGFA165:NRP1 Lu2023 Data")
print(vegfa_nrp1_lu2023_results)

#VEGFA165:NRP1 Herve2008 Data
vegfa_nrp1_herve2008_results = pd.DataFrame({
'2.3nM': [param_vegfa165_nrp1h23_rise[0], param_vegfa165_nrp1h23_rise[1], param_vegfa165_nrp1h23_rise[2], param_vegfa165_nrp1h23_rise[3], param_vegfa165_nrp1h23_rise[4]],
'4.6nM': [param_vegfa165_nrp1h46_rise[0], param_vegfa165_nrp1h46_rise[1], param_vegfa165_nrp1h46_rise[2], param_vegfa165_nrp1h46_rise[3], param_vegfa165_nrp1h46_rise[4]],
'9.2nM': [param_vegfa165_nrp1h92_rise[0], param_vegfa165_nrp1h92_rise[1], param_vegfa165_nrp1h92_rise[2], param_vegfa165_nrp1h92_rise[3], param_vegfa165_nrp1h92_rise[4]],
'19.5nM': [param_vegfa165_nrp1h19_rise[0], param_vegfa165_nrp1h19_rise[1], param_vegfa165_nrp1h19_rise[2], param_vegfa165_nrp1h19_rise[3], param_vegfa165_nrp1h19_rise[4]],
'39nM': [param_vegfa165_nrp1h39_rise[0], param_vegfa165_nrp1h39_rise[1], param_vegfa165_nrp1h39_rise[2], param_vegfa165_nrp1h39_rise[3], param_vegfa165_nrp1h39_rise[4]]})
vegfa_nrp1_herve2008_results.index=['rmax', 'conc', 'KD', 'ka', 'kd']
print("VEGFA165:NRP1 Herve2008 Data")
print(vegfa_nrp1_herve2008_results)