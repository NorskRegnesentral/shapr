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
df_shapley, pred_explain, internal, timing, MSEv = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    predict_model = lambda m, x: m(torch.from_numpy(x.values).float()).cpu().detach().numpy(),
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)
"""
       none    MedInc  HouseAge  AveRooms  AveBedrms  Population  AveOccup  \
1  2.205947  2.313935  5.774470  5.425240   4.194669    1.712164  3.546001   
2  2.205947  4.477620  5.467266  2.904239   3.046492    1.484807  5.631292   
3  2.205946  4.028013  1.168401  5.229893   1.719724    2.134012  3.426378   
4  2.205948  4.230376  8.639265  1.138520   3.776463    3.786978  4.253034   
5  2.205947  3.923747  1.483737  1.113199   4.963213   -3.645875  4.950775   

   Latitude  Longitude  
1  1.102239   2.906469  
2  4.966465   2.178510  
3  3.503413   2.909760  
4  3.413727   3.795563  
5  3.011126   4.016985  
"""

MSEv["MSEv"]
"""
MSEv	MSEv_sd
1	27.046126	7.253933
"""