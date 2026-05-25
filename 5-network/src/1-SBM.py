import graph_tool.all as gt
import pandas as pd
import numpy as np
import os
import pickle

# Load your network
g = gt.load_graph("5-network/data/df_psych_network_largest_comp_nodelist.graphml")
weights = g.ep["weight"]

gt.seed_rng(42)
# Run Bayesian inference for community detection
model_path = "5-network/out/sbm_model.pkl"

# Check if file exists and load the model, otherwise run inference and save the model
if os.path.exists(model_path):
    with open(model_path, "rb") as f:
        state = pickle.load(f)
else:
    state = gt.minimize_nested_blockmodel_dl(
    # Run Bayesian inference for community detection 
    # This takes some time 
        g=g, 
        state_args={"deg_corr": True, 
                    "recs": [weights], 
                    "rec_types": ["real-normal"]}
        )
    ## Save the inferred model
    with open(model_path, "wb") as f:
        import pickle
        pickle.dump(state, f)
# Get number of inferred communities
for level, s in enumerate(state.get_levels()):
    print(f"Level {level}: {s.get_B()} blocks, DL = {s.entropy():.2f}")

levels = state.get_levels()  # NestedBlockState -> list of BlockState
per_level_dl = np.array([s.entropy() for s in levels])
cum_dl = per_level_dl.cumsum()

for i, (dl_i, cdl_i) in enumerate(zip(per_level_dl, cum_dl)):
    print(f"Level {i}: blocks={levels[i].get_B():>6}, level_DL={dl_i:>12.2f}, cumulative_DL={cdl_i:>12.2f}")

best_level = int(cum_dl.argmin())
print(f"\nArgmin cumulative DL at Level {best_level} with {levels[best_level].get_B()} blocks")

# Choose level 0 because it has a reasonable number of communities and interpretability

chosen = state.get_levels()[best_level]  # Level 0 (15240 blocks)
blocks = chosen.get_blocks()
labels = blocks.a

for key, prop in g.vp.items():
    values = [prop[v] for v in g.vertices()][:10]
    print(f"{key}: {values}")
names = [g.vp["name"][v] for v in g.vertices()]

node_list = pd.DataFrame({"node": names, "community": labels})
node_list.to_csv("5-network/out/python_sbm.csv", index=False)

