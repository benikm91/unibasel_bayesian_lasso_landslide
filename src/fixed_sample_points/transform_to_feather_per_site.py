import pickle

from fixed_sample_points.config import *
from util.config import Config

config = Config.default()
output_dir = os.path.join(os.path.dirname(__file__), "out")
debug_dir = os.path.join(output_dir, "debug")

results = list()

for site in config.sites:
    print(f"Generate for {site.name}")
    file_name = f"{site.name}_geo_df"

    # Load data of site
    with open(os.path.join(output_dir, f'{file_name}.pickle'), 'rb') as handle:
        gdf_points = pickle.load(handle)

    path = os.path.join(output_dir, f'{file_name}.feather')
    gdf_points.to_feather(path)
