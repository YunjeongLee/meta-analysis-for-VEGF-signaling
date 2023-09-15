import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from scipy.optimize import minimize
import os
import pandas as pd

# Change the working directory 
print(os.getcwd()) # Prints the current working directory
# Provide the new path here
os.chdir('C:/Users/Imoukhuede lab/OneDrive - UW/Desktop/GitHub/meta-analysis-for-VEGF-signaling/data') 
#C:/Users/lione/Desktop/GitHub/meta-analysis-for-VEGF-signaling/data
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
    decay = data.loc[max_index:]
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
    decay.iloc[:, 0] = decay.iloc[:, 0] - decay.iloc[0, 0]
    globals()[f'{dataset_name}_decay'] = decay

# Define the function 

def fit_functions_decay(data, ru_col):
    r0 = data[ru_col].max() 
    def rt_kd_function(t, kd):
        return (r0*np.exp(-kd*t))
    return rt_kd_function  # Return the function

def fit_functions_rise(data, ru_col, kd):
    rmax = data[ru_col].max()
    conc = float(ru_col[3:-2])*(10**(-9))
    def rt_ka_function(t, ka):
        return ((rmax*conc) / (kd/ka + conc) ) * ( 1 - np.exp((-1*(ka*conc + kd)) *t) )
    return rt_ka_function  # Return the function

# fitting function

def fit_data_rise(data, time_col, ru_col,function, kd):
    param_k, pcov_k = curve_fit(function, data[time_col], data[ru_col], p0=1e7, bounds=(0, np.inf))
    #x = minimize(function,x0=1e7, bounds=(1, np.inf))
    return param_k, pcov_k

datasets_decay = {
    'vegfa165_vegfr2l05_decay': ['Time 0.5nM', 'RU 0.5nM'],
    'vegfa165_vegfr2l1_decay': ['Time 1nM', 'RU 1nM'],
    'vegfa165_vegfr2l2_decay': ['Time 2nM', 'RU 2nM'],
    'vegfa165_vegfr2l4_decay': ['Time 4nM', 'RU 4nM'],
    'vegfa165_vegfr2l8_decay': ['Time 8nM', 'RU 8nM'],

    'vegfa165_nrp1l05_decay': ['Time 0.5nM', 'RU 0.5nM'],
    'vegfa165_nrp1l1_decay': ['Time 1nM', 'RU 1nM'],
    'vegfa165_nrp1l2_decay': ['Time 2nM', 'RU 2nM'],
    'vegfa165_nrp1l4_decay': ['Time 4nM', 'RU 4nM'],
    'vegfa165_nrp1l8_decay': ['Time 8nM', 'RU 8nM'],

    'vegfa165_nrp1h23_decay': ['Time 2.3nM', 'RU 2.3nM'],
    'vegfa165_nrp1h46_decay': ['Time 4.6nM', 'RU 4.6nM'],
    'vegfa165_nrp1h92_decay': ['Time 9.2nM', 'RU 9.2nM'],
    'vegfa165_nrp1h19_decay': ['Time 19.5nM', 'RU 19.5nM'],
    'vegfa165_nrp1h39_decay': ['Time 39nM', 'RU 39nM']
}

for dataset_name, cols in datasets_decay.items():
    time_col, ru_col = cols
    dataset = globals()[dataset_name]
    function = fit_functions_decay(dataset, ru_col)  # Generate the function
    param_d, pcov_d = curve_fit(function, dataset[time_col], dataset[ru_col],p0 = 1e-2, bounds=(0, np.inf))
    print(f'{dataset_name} kd: {param_d[0]}')
    globals()[f'param_{dataset_name}'], globals()[f'pcov_{dataset_name}'] = param_d, pcov_d

# Extract the kd values from the parameters

kd_values = [
    param_vegfa165_vegfr2l05_decay[0],
    param_vegfa165_vegfr2l1_decay[0],
    param_vegfa165_vegfr2l2_decay[0],
    param_vegfa165_vegfr2l4_decay[0],
    param_vegfa165_vegfr2l8_decay[0],
    param_vegfa165_nrp1l05_decay[0],
    param_vegfa165_nrp1l1_decay[0],
    param_vegfa165_nrp1l2_decay[0],
    param_vegfa165_nrp1l4_decay[0],
    param_vegfa165_nrp1l8_decay[0],
    param_vegfa165_nrp1h23_decay[0],
    param_vegfa165_nrp1h46_decay[0],
    param_vegfa165_nrp1h92_decay[0],
    param_vegfa165_nrp1h19_decay[0],
    param_vegfa165_nrp1h39_decay[0]
]

datasets_rise = {
    'vegfa165_vegfr2l05_rise': ['Time 0.5nM', 'RU 0.5nM', fit_functions_rise],
    'vegfa165_vegfr2l1_rise': ['Time 1nM', 'RU 1nM', fit_functions_rise],
    'vegfa165_vegfr2l2_rise': ['Time 2nM', 'RU 2nM', fit_functions_rise],
    'vegfa165_vegfr2l4_rise': ['Time 4nM', 'RU 4nM', fit_functions_rise],
    'vegfa165_vegfr2l8_rise': ['Time 8nM', 'RU 8nM', fit_functions_rise],
    
    'vegfa165_nrp1l05_rise': ['Time 0.5nM', 'RU 0.5nM', fit_functions_rise],
    'vegfa165_nrp1l1_rise': ['Time 1nM', 'RU 1nM', fit_functions_rise],
    'vegfa165_nrp1l2_rise': ['Time 2nM', 'RU 2nM', fit_functions_rise],
    'vegfa165_nrp1l4_rise': ['Time 4nM', 'RU 4nM', fit_functions_rise],
    'vegfa165_nrp1l8_rise': ['Time 8nM', 'RU 8nM', fit_functions_rise],

    'vegfa165_nrp1h23_rise': ['Time 2.3nM', 'RU 2.3nM', fit_functions_rise],
    'vegfa165_nrp1h46_rise': ['Time 4.6nM', 'RU 4.6nM', fit_functions_rise],
    'vegfa165_nrp1h92_rise': ['Time 9.2nM', 'RU 9.2nM', fit_functions_rise],
    'vegfa165_nrp1h19_rise': ['Time 19.5nM', 'RU 19.5nM', fit_functions_rise],
    'vegfa165_nrp1h39_rise': ['Time 39nM', 'RU 39nM', fit_functions_rise]
}

for i, (dataset_name, cols) in enumerate(datasets_rise.items()):
    time_col, ru_col, function_generator  = cols
    dataset = globals()[dataset_name]

    function = function_generator(dataset,ru_col ,kd_values[i])  # Generate the function
    param, pcov = fit_data_rise(dataset, time_col, ru_col, function, kd_values[i])
    globals()[f'param_{dataset_name}'], globals()[f'pcov_{dataset_name}'] = param, pcov

# Plot the data

# Define a function to plot the data
def plot_data(data_rise, time_col, ru_col, function, param_rise, label):
    plt.scatter(data_rise[time_col], data_rise[ru_col], label=f'{label} Data')
    plt.plot(data_rise[time_col], function(data_rise,ru_col)(data_rise[time_col], *param_rise), 'r-', label=f'{label} Fit')

# Define the datasets and parameters for the decay data
dataset_vrl_decay = {
    'vegfa165_vegfr2l05': [vegfa165_vegfr2l05_decay, "Time 0.5nM", "RU 0.5nM", fit_functions_decay, param_vegfa165_vegfr2l05_decay, '0.5nM'],
    'vegfa165_vegfr2l1': [vegfa165_vegfr2l1_decay, "Time 1nM", "RU 1nM", fit_functions_decay, param_vegfa165_vegfr2l1_decay, '1nM'],
    'vegfa165_vegfr2l2': [vegfa165_vegfr2l2_decay, "Time 2nM", "RU 2nM", fit_functions_decay, param_vegfa165_vegfr2l2_decay, '2nM'],
    'vegfa165_vegfr2l4': [vegfa165_vegfr2l4_decay, "Time 4nM", "RU 4nM", fit_functions_decay, param_vegfa165_vegfr2l4_decay, '4nM'],
    'vegfa165_vegfr2l8': [vegfa165_vegfr2l8_decay, "Time 8nM", "RU 8nM", fit_functions_decay, param_vegfa165_vegfr2l8_decay, '8nM'],
}

dataset_vnh_decay = {
    'vegfa165_nrp1l05': [vegfa165_nrp1l05_decay, "Time 0.5nM", "RU 0.5nM", fit_functions_decay, param_vegfa165_nrp1l05_decay, '0.5nM'],
    'vegfa165_nrp1l1': [vegfa165_nrp1l1_decay, "Time 1nM", "RU 1nM", fit_functions_decay, param_vegfa165_nrp1l1_decay, '1nM'],
    'vegfa165_nrp1l2': [vegfa165_nrp1l2_decay, "Time 2nM", "RU 2nM", fit_functions_decay, param_vegfa165_nrp1l2_decay, '2nM'],
    'vegfa165_nrp1l4': [vegfa165_nrp1l4_decay, "Time 4nM", "RU 4nM", fit_functions_decay, param_vegfa165_nrp1l4_decay, '4nM'],
    'vegfa165_nrp1l8': [vegfa165_nrp1l8_decay, "Time 8nM", "RU 8nM", fit_functions_decay, param_vegfa165_nrp1l8_decay, '8nM'],
}

dataset_vrh_decay = {
    'vegfa165_nrp1h23': [vegfa165_nrp1h23_decay, "Time 2.3nM", "RU 2.3nM", fit_functions_decay, param_vegfa165_nrp1h23_decay, '2.3nM'],
    'vegfa165_nrp1h46': [vegfa165_nrp1h46_decay, "Time 4.6nM", "RU 4.6nM", fit_functions_decay, param_vegfa165_nrp1h46_decay, '4.6nM'],
    'vegfa165_nrp1h92': [vegfa165_nrp1h92_decay, "Time 9.2nM", "RU 9.2nM", fit_functions_decay, param_vegfa165_nrp1h92_decay, '9.2nM'],
    'vegfa165_nrp1h19': [vegfa165_nrp1h19_decay, "Time 19.5nM", "RU 19.5nM", fit_functions_decay, param_vegfa165_nrp1h19_decay, '19.5nM'],
    'vegfa165_nrp1h39': [vegfa165_nrp1h39_decay, "Time 39nM", "RU 39nM", fit_functions_decay, param_vegfa165_nrp1h39_decay, '39nM']
}

# Plot the data for each dissociation dataset

plt.figure()
for dataset_name, params in dataset_vrl_decay.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:VEGFR2 Lu2023 dissociation Data')
plt.show()

plt.figure()
for dataset_name, params in dataset_vnh_decay.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Lu2023 dissociation Data')
plt.show()

plt.figure()
for dataset_name, params in dataset_vrh_decay.items():
    plot_data(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Herve2008 dissociation Data')
plt.show()


# Define the datasets and parameters for the rise data
dataset_vrl = {
    'vegfa165_vegfr2l05': [vegfa165_vegfr2l05_rise, "Time 0.5nM", "RU 0.5nM", fit_functions_rise, param_vegfa165_vegfr2l05_rise, '0.5nM'],
    'vegfa165_vegfr2l1': [vegfa165_vegfr2l1_rise, "Time 1nM", "RU 1nM", fit_functions_rise, param_vegfa165_vegfr2l1_rise, '1nM'],
    'vegfa165_vegfr2l2': [vegfa165_vegfr2l2_rise, "Time 2nM", "RU 2nM", fit_functions_rise, param_vegfa165_vegfr2l2_rise, '2nM'],
    'vegfa165_vegfr2l4': [vegfa165_vegfr2l4_rise, "Time 4nM", "RU 4nM", fit_functions_rise, param_vegfa165_vegfr2l4_rise, '4nM'],
    'vegfa165_vegfr2l8': [vegfa165_vegfr2l8_rise, "Time 8nM", "RU 8nM", fit_functions_rise, param_vegfa165_vegfr2l8_rise, '8nM'],
}

dataset_vnh = {
    'vegfa165_nrp1l05': [vegfa165_nrp1l05_rise, "Time 0.5nM", "RU 0.5nM", fit_functions_rise, param_vegfa165_nrp1l05_rise, '0.5nM'],
    'vegfa165_nrp1l1': [vegfa165_nrp1l1_rise, "Time 1nM", "RU 1nM", fit_functions_rise, param_vegfa165_nrp1l1_rise, '1nM'],
    'vegfa165_nrp1l2': [vegfa165_nrp1l2_rise, "Time 2nM", "RU 2nM", fit_functions_rise, param_vegfa165_nrp1l2_rise, '2nM'],
    'vegfa165_nrp1l4': [vegfa165_nrp1l4_rise, "Time 4nM", "RU 4nM", fit_functions_rise, param_vegfa165_nrp1l4_rise, '4nM'],
    'vegfa165_nrp1l8': [vegfa165_nrp1l8_rise, "Time 8nM", 'RU 8nM', fit_functions_rise, param_vegfa165_nrp1l8_rise, '8nM'],
}

dataset_vrh = {
    'vegfa165_nrp1h23': [vegfa165_nrp1h23_rise, "Time 2.3nM", "RU 2.3nM", fit_functions_rise, param_vegfa165_nrp1h23_rise, '2.3nM'],
    'vegfa165_nrp1h46': [vegfa165_nrp1h46_rise, "Time 4.6nM", "RU 4.6nM", fit_functions_rise, param_vegfa165_nrp1h46_rise, '4.6nM'],
    'vegfa165_nrp1h92': [vegfa165_nrp1h92_rise, "Time 9.2nM", "RU 9.2nM", fit_functions_rise, param_vegfa165_nrp1h92_rise, '9.2nM'],
    'vegfa165_nrp1h19': [vegfa165_nrp1h19_rise, "Time 19.5nM", "RU 19.5nM",fit_functions_rise, param_vegfa165_nrp1h19_rise, '19.5nM'],
    'vegfa165_nrp1h39': [vegfa165_nrp1h39_rise, "Time 39nM", "RU 39nM", fit_functions_rise, param_vegfa165_nrp1h39_rise, '39nM']    
}

# Plot the data for each association dataset

def plot_datar(data_rise, time_col, ru_col, function, param_rise, label):
    plt.scatter(data_rise[time_col], data_rise[ru_col], label=f'{label} Data')
    plt.plot(data_rise[time_col], function(data_rise,ru_col,kd_values[counter])(data_rise[time_col], *param_rise), 'r-', label=f'{label} Fit')

plt.figure()
counter = 0
for dataset_name, params in dataset_vrl.items(): 
    counter += 1
    plot_datar(*params)
    plt.legend()
plt.title('VEGFA165:VEGFR2 Lu2023 association Data')
plt.show()
print(counter)

plt.figure()
counter = 5
for dataset_name, params in dataset_vnh.items():
    plot_datar(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Lu2023 association Data')
plt.show()

plt.figure()
counter = 10
for dataset_name, params in dataset_vrh.items():
    plot_datar(*params)
    plt.legend()
plt.title('VEGFA165:NRP1 Herve2008 association Data')
plt.show()

# Generate a dataframe with the parameters for each dataset

def generate_dataframe(param_list, index):
    data = {}
    for param in param_list:
        data[param[0]] = param[1]
    df = pd.DataFrame(data)
    df.index = index
    return df

# Generate a dataframe with the parameters for each dataset

# Rise parameters
#VEGFA165:VEGFR2 Lu2023 Data
vegfa_vegf2_lu2023_results = generate_dataframe([
    ('0.5nM', param_vegfa165_vegfr2l05_rise),
    ('1nM', param_vegfa165_vegfr2l1_rise),
    ('2nM', param_vegfa165_vegfr2l2_rise),
    ('4nM', param_vegfa165_vegfr2l4_rise),
    ('8nM', param_vegfa165_vegfr2l8_rise)
], ['ka'])
vegfa_vegf2_lu2023_results['mean'] = vegfa_vegf2_lu2023_results.mean(axis=1)
vegfa_vegf2_lu2023_results['std'] = vegfa_vegf2_lu2023_results.apply(np.std, axis=1)
print("VEGFA165:VEGFR2 Lu2023 association Data")
display(vegfa_vegf2_lu2023_results)

#VEGFA165:NRP1 Lu2023 Data
vegfa_nrp1_lu2023_results = generate_dataframe([
    ('0.5nM', param_vegfa165_nrp1l05_rise),
    ('1nM', param_vegfa165_nrp1l1_rise),
    ('2nM', param_vegfa165_nrp1l2_rise),
    ('4nM', param_vegfa165_nrp1l4_rise),
    ('8nM', param_vegfa165_nrp1l8_rise)
], ['ka'])
vegfa_nrp1_lu2023_results['mean'] = vegfa_nrp1_lu2023_results.mean(axis=1)
vegfa_nrp1_lu2023_results['std'] = vegfa_nrp1_lu2023_results.apply(np.std, axis=1)
print("VEGFA165:NRP1 Lu2023 association Data")
display(vegfa_nrp1_lu2023_results)

#VEGFA165:NRP1 Herve2008 Data
vegfa_nrp1_herve2008_results = generate_dataframe([
    ('2.3nM', param_vegfa165_nrp1h23_rise),
    ('4.6nM', param_vegfa165_nrp1h46_rise),
    ('9.2nM', param_vegfa165_nrp1h92_rise),
    ('19.5nM', param_vegfa165_nrp1h19_rise),
    ('39nM', param_vegfa165_nrp1h39_rise)
], ['ka'])
vegfa_nrp1_herve2008_results['mean'] = vegfa_nrp1_herve2008_results.mean(axis=1)
vegfa_nrp1_herve2008_results['std'] = vegfa_nrp1_herve2008_results.apply(np.std, axis=1)
print("VEGFA165:NRP1 Herve2008 association Data")
display(vegfa_nrp1_herve2008_results)

# Decay parameters
#VEGFA165:VEGFR2 Lu2023 Data
vegfa_vegf2_lu2023_results = generate_dataframe([
    ('0.5nM', param_vegfa165_vegfr2l05_decay),
    ('1nM', param_vegfa165_vegfr2l1_decay),
    ('2nM', param_vegfa165_vegfr2l2_decay),    
    ('4nM', param_vegfa165_vegfr2l4_decay),
    ('8nM', param_vegfa165_vegfr2l8_decay)
], ['kd'])
vegfa_vegf2_lu2023_results['mean'] = vegfa_vegf2_lu2023_results.mean(axis=1)
vegfa_vegf2_lu2023_results['std'] = vegfa_vegf2_lu2023_results.apply(np.std, axis=1)
print("VEGFA165:VEGFR2 Lu2023 dissociation Data")
display(vegfa_vegf2_lu2023_results)

#VEGFA165:NRP1 Lu2023 Data
vegfa_nrp1_lu2023_results = generate_dataframe([
    ('0.5nM', param_vegfa165_nrp1l05_decay),
    ('1nM', param_vegfa165_nrp1l1_decay),
    ('2nM', param_vegfa165_nrp1l2_decay),
    ('4nM', param_vegfa165_nrp1l4_decay),
    ('8nM', param_vegfa165_nrp1l8_decay)
], ['kd'])
vegfa_nrp1_lu2023_results['mean'] = vegfa_nrp1_lu2023_results.mean(axis=1)
vegfa_nrp1_lu2023_results['std'] = vegfa_nrp1_lu2023_results.apply(np.std, axis=1)
print("VEGFA165:NRP1 Lu2023 dissociation Data")
display(vegfa_nrp1_lu2023_results)

#VEGFA165:NRP1 Herve2008 Data
vegfa_nrp1_herve2008_results = generate_dataframe([
    ('2.3nM', param_vegfa165_nrp1h23_decay),
    ('4.6nM', param_vegfa165_nrp1h46_decay),
    ('9.2nM', param_vegfa165_nrp1h92_decay),
    ('19.5nM', param_vegfa165_nrp1h19_decay),
    ('39nM', param_vegfa165_nrp1h39_decay)
], ['kd'])
vegfa_nrp1_herve2008_results['mean'] = vegfa_nrp1_herve2008_results.mean(axis=1)
vegfa_nrp1_herve2008_results['std'] = vegfa_nrp1_herve2008_results.apply(np.std, axis=1)
print("VEGFA165:NRP1 Herve2008 dissociation Data")
display(vegfa_nrp1_herve2008_results)
