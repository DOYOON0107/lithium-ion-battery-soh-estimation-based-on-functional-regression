# 1. library import
import pandas as pd
from scipy.io import loadmat
import datetime

# 2. convert matdata to dataframe
path = "C:/Users/newld/PycharmProjects/Li-ion Battery Prediction based on Deep Neural Network/Preprocessing data/Battery[05,06,07,18]/"
def load_discharge_data(battery):
    mat = loadmat(path + battery + '.mat')
    dataset = []
    counter = 0

    for i in range(len(mat[battery][0, 0]['cycle'][0])):
        row = mat[battery][0, 0]['cycle'][0, i]
        if row['type'][0] == 'discharge':
            ambient_temp = row['ambient_temperature'][0][0]
            date_time = datetime.datetime(*[int(t) for t in row['time'][0][:5]]) + datetime.timedelta(seconds=int(row['time'][0][5]))
            data = row['data']
            capacity = data[0][0]['Capacity'][0][0]

            for j in range(len(data[0][0]['Voltage_measured'][0])):
                dataset.append([
                    counter + 1,
                    ambient_temp,
                    date_time,
                    capacity,
                    data[0][0]['Voltage_measured'][0][j],
                    data[0][0]['Current_measured'][0][j],
                    data[0][0]['Temperature_measured'][0][j],
                    data[0][0]['Current_load'][0][j],
                    data[0][0]['Voltage_load'][0][j],

                    data[0][0]['Time'][0][j]
                ])
            counter += 1

    return pd.DataFrame(dataset, columns=[
        'cycle', 'ambient_temperature', 'date_time', 'capacity',
        'voltage_measured', 'current_measured', 'temperature_measured',
        'current_load', 'voltage_load', 'time'
    ])


# 3. convert matdata to dataframe
def load_charge_data(battery):
    mat = loadmat(path + battery + '.mat')
    dataset = []
    counter = 0

    for i in range(len(mat[battery][0, 0]['cycle'][0])):
        row = mat[battery][0, 0]['cycle'][0, i]
        if row['type'][0] == 'charge':
            ambient_temp = row['ambient_temperature'][0][0]
            date_time = datetime.datetime(*[int(t) for t in row['time'][0][:5]]) + datetime.timedelta(seconds=int(row['time'][0][5]))
            data = row['data']

            for j in range(len(data[0][0]['Voltage_measured'][0])):
                dataset.append([
                    counter + 1,
                    ambient_temp,
                    date_time,
                    data[0][0]['Voltage_measured'][0][j],
                    data[0][0]['Current_measured'][0][j],
                    data[0][0]['Temperature_measured'][0][j],
                    data[0][0]['Current_charge'][0][j],
                    data[0][0]['Voltage_charge'][0][j],
                    data[0][0]['Time'][0][j]
                ])
            counter += 1

    return pd.DataFrame(dataset, columns=[
        'cycle', 'ambient_temperature', 'date_time',
        'voltage_measured', 'current_measured', 'temperature_measured',
        'current_charge', 'voltage_charge', 'time'
    ])


# data import
battery_list = ["B0005", "B0006", "B0007", "B0018"]


for battery in battery_list:
    discharge_df = load_discharge_data(battery)
    charge_df = load_charge_data(battery)
    discharge_df.to_csv(path + battery +'_discharge' + '.csv', index=False)
    charge_df.to_csv(path + battery + '_charge' + '.csv', index=False)
    print(battery + "_complete")
