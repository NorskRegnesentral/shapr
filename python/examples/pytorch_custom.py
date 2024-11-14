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

for epoch in range(5):
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
)
print(explanation["shapley_values_est"])
"""
   explain_id      none    MedInc   HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205951  3.531437   7.746453  6.985043   5.454877    3.287326   
2           2  2.205951  6.004403   7.041080  4.254553   4.118677    3.162567   
3           3  2.205950  5.497648   1.538680  6.750968   2.806428    3.687014   
4           4  2.205951  5.761901  11.378609  2.112351   5.013451    5.754630   
5           5  2.205951  5.325281   2.585713  2.224409   6.418153   -2.848570   

   AveOccup  Latitude  Longitude  
1  4.774873  2.273699   4.314784  
2  7.386783  6.473623   3.318631  
3  5.193341  4.875864   4.290797  
4  5.866562  4.564957   5.139962  
5  6.428984  4.280456   5.509226  
"""

print(explanation["shapley_values_sd"])

"""
   explain_id          none    MedInc  HouseAge  AveRooms  AveBedrms  \
1           1  3.523652e-08  0.122568  0.124885  0.163694   0.134910   
2           2  3.501778e-08  0.125286  0.113064  0.123057   0.129869   
3           3  1.805247e-08  0.098208  0.095959  0.115399   0.102265   
4           4  3.227380e-08  0.110442  0.118524  0.124688   0.101476   
5           5  3.650380e-08  0.125538  0.130427  0.136797   0.131515   

   Population  AveOccup  Latitude  Longitude  
1    0.133510  0.149141  0.132394   0.121605  
2    0.113429  0.124539  0.122773   0.100871  
3    0.092633  0.110790  0.090657   0.090542  
4    0.114721  0.122266  0.103081   0.105613  
5    0.113853  0.139291  0.135377   0.132476  
"""

explanation["MSEv"]["MSEv"]
"""
	MSEv	MSEv_sd
1	33.143896	7.986808
"""
