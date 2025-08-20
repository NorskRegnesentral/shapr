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
1           1  2.205938 -0.391947 -0.091373 -0.188752   0.068620    0.003777
2           2  2.205938 -0.365962  0.039864 -0.113096  -0.177978    0.056510
3           3  2.205938  0.053568  0.168420 -0.150791  -0.075012   -0.001990
4           4  2.205938  0.289718 -0.184352  0.114339   0.055226   -0.076613
5           5  2.205938 -0.356045  0.090436 -0.045120   0.075531    0.067988

   AveOccup  Latitude  Longitude
1 -0.061825 -0.072816  -0.115482
2  0.048095 -0.107911  -0.066225
3 -0.230500  0.070272   0.096570
4 -0.019366  0.004216  -0.016615
5 -0.038346 -0.102561   0.161560
"""

print(explanation["shapley_values_sd"])

"""
   explain_id  none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1   0.0  0.007140  0.007088  0.008192   0.008615    0.007520
2           2   0.0  0.012215  0.012000  0.011966   0.012511    0.011412
3           3   0.0  0.007720  0.008195  0.008505   0.009778    0.008609
4           4   0.0  0.005611  0.005623  0.005926   0.004908    0.005962
5           5   0.0  0.009542  0.009532  0.010313   0.009491    0.008562

   AveOccup  Latitude  Longitude
1  0.008217  0.007403   0.006449
2  0.011162  0.013482   0.011676
3  0.008251  0.009447   0.009556
4  0.004946  0.004537   0.005202
5  0.008860  0.010345   0.009153
"""

print(explanation["MSEv"]["MSEv"])
"""
      MSEv   MSEv_sd
1  0.08098  0.014908
"""
