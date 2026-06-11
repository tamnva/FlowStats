import torch
import numpy as np
from pathlib import Path
from hydroecolstm_lite.data.read_config import read_config
from hydroecolstm_lite.data.read_data import combine_timeseries_static
from hydroecolstm_lite.data.read_data import read_scale_inference_data
from hydroecolstm_lite.model.create_model import create_model

# Read configuration file, please modify the path to the config.yml file
lstm_data_dir = "paste_lstm_data_dir_here"

# Read and update config file
config = read_config(Path(lstm_data_dir, "config.yml"))
config["timeseries_data_file"][0] = Path(lstm_data_dir, "time_series.csv")
config["static_data_file"][0] = Path(lstm_data_dir, "static_attributes.csv")
config["timeseries_data_file_inference"] = config["timeseries_data_file"]
config["static_data_file_inference"] = config["static_data_file"]
config["output_directory"][0] = Path(lstm_data_dir)

#-----------------------------------------------------------------------------#
#                              Inference data                                 #
#-----------------------------------------------------------------------------#
# Load scaler
scaler = torch.load(Path(lstm_data_dir, "scaler.pt"), 
                    weights_only=False)

# Create model 
model = create_model(config)

# Load state dict
state_dict = torch.load(
    Path(lstm_data_dir, "best_model_state_dict.pt"), 
    weights_only=False)

model.load_state_dict(state_dict)

# Read and scale inference data
inference_data = read_scale_inference_data(config, scaler)

inference_data = combine_timeseries_static(
    inference_data["inference_timeseries_data"], 
    inference_data["inference_static_data"], 
    model)

simulated = inference_data[["id", "time"]].copy()
simulated["discharge_spec_obs"] = np.nan

inference_data = torch.tensor(
    inference_data[model.input_features].values,
    dtype=torch.float32
    )

inference_data = torch.split(
    inference_data, 
    int(inference_data.shape[0]/len(config["id_inference"])), 
    dim=0
    )    
    
model.eval()
with torch.inference_mode():
    for ids, chunk in zip(simulated["id"].unique().tolist(), inference_data):
        mask = simulated["id"] == ids 
        simulated.loc[mask, "discharge_spec_obs"] = (
            model(chunk).squeeze().detach().numpy())

scaler["timeseries_data"].inverse(simulated).to_csv(
    Path(lstm_data_dir, "de_sim_discharge_update.csv"), 
    index=False)

#-----------------------------------------------------------------------------#
#             The code within this section is used for training               #
#-----------------------------------------------------------------------------#
'''
from hydroecolstm_lite.model_run import run_config
from hydroecolstm_lite.utility.evaluation_function import nse

# Just train for scratch, don't use initial state dict
del config["init_model_state_dict"]

data_scaled, scaler, model, trainer = run_config(config)

# Combine time series and statics
test_data = combine_timeseries_static(
    data_scaled['timeseries_data_test'], 
    data_scaled['static_data'], model
    )

# Create dataframe to store results from test data
simulated = test_data[["id", "time"]].copy()
simulated["discharge_spec_obs"] = np.nan

# Convert test data to model input
test_data = torch.tensor(
    test_data[model.input_features].values, 
    dtype=torch.float32
    )

# Split to chunk to save memory
test_data = torch.split(
    test_data, 
    int(test_data.shape[0]/len(config["id_test"])), 
    dim=0
    )    
    
# Now forward pass to get simulated streamflow (scaled data)
model.eval()
with torch.inference_mode():
    for ids, chunk in zip(simulated["id"].unique().tolist(), test_data):
        print(ids)
        mask = simulated["id"] == ids 
        simulated.loc[mask, "discharge_spec_obs"] = (
            model(chunk).squeeze().detach().numpy())

# Get NSE test period for each basins
data_scaled['timeseries_data_test']["simulated"] = (
    simulated["discharge_spec_obs"]
    )

nse_val = (data_scaled['timeseries_data_test'].
 groupby("id", observed=True).
 apply(lambda g: nse(g["simulated"], g["discharge_spec_obs"], 
                     config["warmup_length"]), include_groups=False).
 rename("nse").reset_index())
nse_val["nse"].median()

# Save simulated streamflow (transform back to original value mm/day)
scaler["timeseries_data"].inverse(simulated).to_csv(
    Path(lstm_data_dir, "test_data.csv"), index=False
    )
'''