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
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205948  3.516491  4.069221  4.807392   4.162077    3.419979   
2           2  2.205948  4.452062  4.868121  3.657762   3.441219    3.445877   
3           3  2.205948  4.242527  4.834834  4.847549   2.698246    3.597892   
4           4  2.205948  4.196727  5.077788  2.831637   3.938577    4.289322   
5           5  2.205948  4.115855  3.217642  2.986512   4.584130    1.320592   

   AveOccup  Latitude  Longitude  
1  4.158741  2.698329   3.695627  
2  4.724255  4.478227   3.536784  
3  3.934142  4.203065   4.175473  
4  4.184477  3.914167   4.245993  
5  4.629850  3.457693   4.190717  
"""

print(explanation["shapley_values_sd"])

"""
   explain_id          none    MedInc  HouseAge  AveRooms  AveBedrms  \
1           1  2.128999e-08  0.095949  0.121400  0.109478   0.089252   
2           2  1.230629e-08  0.073107  0.079607  0.069861   0.071715   
3           3  1.451430e-08  0.072112  0.081075  0.066201   0.063103   
4           4  1.736058e-08  0.086020  0.093031  0.095717   0.082699   
5           5  1.903768e-08  0.087028  0.097773  0.087086   0.096578   

   Population  AveOccup  Latitude  Longitude  
1    0.097459  0.098798  0.128215   0.102793  
2    0.068862  0.074687  0.083260   0.073188  
3    0.065732  0.072440  0.073805   0.066479  
4    0.079885  0.088326  0.102809   0.084057  
5    0.091073  0.099251  0.097776   0.098360  
"""

explanation["MSEv"]["MSEv"]
"""
MSEv	MSEv_sd
1	5.122576	0.874736
"""
