import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.optim import Adam
from torch.utils.data import TensorDataset, DataLoader
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model (not seed controlled here)
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
explanation.print()

# Note: The output will vary due to non-deterministic training
"""
   explain_id  none   MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>    <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.17170  -0.2730  -0.1190    0.0528    -0.0433   0.0437
2:          2  2.21 -0.16897   0.0771  -0.1292   -0.0605    -0.0175   0.0137
3:          3  2.21  0.00656   0.6023  -0.0506   -0.1515    -0.1412  -0.1536
4:          4  2.21  0.12541  -0.4635   0.1106    0.0204     0.0225  -0.0486
5:          5  2.21 -0.15517   0.1134  -0.0381    0.0681    -0.1981   0.0350
   Latitude Longitude
      <num>     <num>
1:  -0.2604  -0.01446
2:   0.0910   0.00575
3:   0.0623   0.09130
4:   0.1160   0.16302
5:  -0.1024  -0.02871
"""

explanation.print("shapley_sd")

"""
   explain_id  none  MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>   <num>    <num>    <num>     <num>      <num>    <num>
1:          1     0 0.00541  0.00563  0.00432   0.00394    0.00409  0.00324
2:          2     0 0.00531  0.00607  0.00548   0.00669    0.00905  0.00457
3:          3     0 0.00644  0.00495  0.00661   0.00519    0.00477  0.00481
4:          4     0 0.00392  0.00388  0.00440   0.00365    0.00370  0.00382
5:          5     0 0.00437  0.00560  0.00594   0.00450    0.00483  0.00395
   Latitude Longitude
      <num>     <num>
1:  0.00360   0.00462
2:  0.00799   0.00686
3:  0.00487   0.00558
4:  0.00416   0.00345
5:  0.00535   0.00485
"""

explanation.print("MSEv")


"""
    MSEv MSEv_sd
   <num>   <num>
1: 0.124  0.0335
"""
