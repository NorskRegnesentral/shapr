import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.optim import Adam
from torch.utils.data import TensorDataset, DataLoader
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
class MyNeuralNet(nn.Module):

    def __init__(self, in_dim, hidden_dim):
        super().__init__()
        self.lin0 = nn.Linear(in_dim, hidden_dim)
        self.lin1 = nn.Linear(hidden_dim, 1)

    def forward(self, x):
        x = self.lin0(x)
        x = F.relu(x)
        return self.lin1(x).squeeze(-1)

data = TensorDataset(torch.from_numpy(dfx_train.values).float(), torch.from_numpy(dfy_train.values.flatten()).float())
data_loader = DataLoader(data, batch_size=128, shuffle=True)
model = MyNeuralNet(dfx_train.shape[-1], hidden_dim=128)
optim = Adam(model.parameters(), lr=1e-3)

for epoch in range(200):
    print(f'Training, epoch {epoch+1}...')
    for x, y in data_loader:
        p = model(x)
        loss = F.mse_loss(p, y)
        loss.backward()
        optim.step()
        optim.zero_grad()

## Shapr
explanation = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    predict_model = lambda m, x: m(torch.from_numpy(x.values).float()).cpu().detach().numpy(),
    phi0 = dfy_train.mean().item(),
    seed = 1
)
print(explanation["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -0.100550 -0.380012 -0.041632   0.017294    0.014930   
2           2  2.205938 -0.101279  0.058382 -0.104666  -0.080175    0.010757   
3           3  2.205938  0.047357  0.849644 -0.009851  -0.188181    0.046827   
4           4  2.205938  0.084870 -0.574709  0.039377  -0.018821   -0.085610   
5           5  2.205938 -0.121435  0.055792  0.000925   0.029481    0.094140   

   AveOccup  Latitude  Longitude  
1  0.026661 -0.144981  -0.027903  
2 -0.000899  0.029101   0.015536  
3  0.045139  0.110285   0.188362  
4 -0.059465  0.099350   0.138121  
5  0.076669 -0.038006   0.051082  
"""

print(explanation["shapley_values_sd"])

"""
   explain_id          none    MedInc  HouseAge  AveRooms  AveBedrms  \
1           1  4.357423e-09  0.012778  0.012477  0.013521   0.010329   
2           2  7.838904e-09  0.017437  0.017148  0.018990   0.022843   
3           3  4.880924e-09  0.017282  0.025252  0.013946   0.018082   
4           4  3.751068e-09  0.008843  0.009438  0.009137   0.009434   
5           5  5.801988e-09  0.007623  0.009820  0.009337   0.008560   

   Population  AveOccup  Latitude  Longitude  
1    0.010671  0.010003  0.016718   0.013488  
2    0.017491  0.020695  0.058027   0.020306  
3    0.013514  0.017621  0.021226   0.015194  
4    0.009592  0.008193  0.013764   0.008483  
5    0.007934  0.009763  0.014770   0.007096  
"""

print(explanation["MSEv"]["MSEv"])
"""
       MSEv   MSEv_sd
1  0.191949  0.096551
"""
