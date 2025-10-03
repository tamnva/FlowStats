#-----------------------------------------------------------------------------#
#                               Import required packages                      #
#-----------------------------------------------------------------------------#
import torch
import pandas as pd
from pathlib import Path
from hydroecolstm.data.read_data import read_forecast_data
from hydroecolstm.data.read_config import read_config
from hydroecolstm.model.create_model import create_model

# This line will be changed by R to the folder of FlowStats package data
lstm_data_dir = 'change_to_r_library/FlowStats/FlowStats/data/lstm_data'

#-----------------------------------------------------------------------------#
#                  Rerun the model with new meterological data                #
#-----------------------------------------------------------------------------#
config = read_config(Path(lstm_data_dir, 'config.yml'))
config['dynamic_data_file'] = [Path(lstm_data_dir, 'time_series.csv')]
config['static_data_file'] = [Path(lstm_data_dir, 'static_attributes.csv')]
model = create_model(config)
model.load_state_dict(torch.load(Path(lstm_data_dir,'model_state_dict.pt')))

x_scaler = torch.load(Path(lstm_data_dir,'x_scaler.pt'), weights_only=False)
y_scaler = torch.load(Path(lstm_data_dir,'y_scaler.pt'), weights_only=False)

# Rerun the model for entire time series data (need to use forcast mode)
data = read_forecast_data(config)
q_sim = y_scaler.inverse(model.evaluate(x_scaler.transform(data['x_forecast'])))

#-----------------------------------------------------------------------------#
#                  Save the simulated discharge data                          #
#-----------------------------------------------------------------------------#
for objectid in config['object_id_forecast']:
    
    q_sim[objectid][q_sim[objectid] < 0] = 0.01
    temp = pd.DataFrame({'gauge_id': objectid,
                               'date':data['time_forecast'][objectid],
                               'q_mm_day': q_sim[objectid].numpy().flatten()})
    
    if objectid == config['object_id_forecast'][0]: 
        de_sim_discharge = temp
    else:
        de_sim_discharge = pd.concat([de_sim_discharge, temp])
    
de_sim_discharge.to_csv(Path(lstm_data_dir,'de_sim_discharge_update.csv'), sep=',', 
                        header=True, index=False, float_format='%.2f')