"""
Based on landslide points and (fixed) random non landslide points,
this script generates a geopandas dataframe for each site.
"""

import os
import pickle

import geopandas as gpd
import numpy as np
import pandas as pd
import rasterio.mask
from scipy import ndimage

from fixed_sample_points.config import points_dir
from util.config import Config

np.random.seed(42)

config = Config.default()
output_dir = os.path.join(os.path.dirname(__file__), "out")


def handle_missing_values(value):
    # Missing values are encoded as float.min in .tif files -> Replace them with NaN
    return value if value > -3e-37 else np.nan


for site in config.sites:
    df_landslide_points = pd.read_csv(os.path.join(points_dir, f"{site.name}_Landslide_Points_4sqm.txt"))
    df_no_landslide_points = pd.read_csv(os.path.join(points_dir, f"{site.name}_Random_Points_nonLS_4sqm.txt"))

    df_points = pd.concat([df_landslide_points, df_no_landslide_points], axis=0)
    gdf_points = gpd.GeoDataFrame(
        df_points,
        geometry=gpd.points_from_xy(df_points.x, df_points.y),
    )
    gdf_points = gdf_points.set_crs('epsg:2056')

    print(f"Process {site.name} with {gdf_points.shape[0]} points")

    elevation = rasterio.open(os.path.join(config.elevation, f"DEM_2m_{site.name}.tif"))
    slope = rasterio.open(os.path.join(config.slope, f"Slope_2m_{site.name}.tif"))
    aspect = rasterio.open(os.path.join(config.aspect, f"Aspect_2m_{site.name}.tif"))
    twi = rasterio.open(os.path.join(config.topo_wetness_index, f"TWI_2m_{site.name}.tif"))
    flowacc = rasterio.open(os.path.join(config.flow_accumulation, f"FlowAcc_2m_{site.name}.tif"))
    flowdir = rasterio.open(os.path.join(config.flow_direction, f"FlowDir_2m_{site.name}.tif"))
    curvature_plan = rasterio.open(os.path.join(config.curvature, f"Curvature_plan_2m_{site.name}.tif"))
    curvature_profile = rasterio.open(os.path.join(config.curvature, f"Curvature_profile_2m_{site.name}.tif"))

    max_precip_5Y = rasterio.open(os.path.join(config.precip_5Y, f"Max_CHELSA_LV95_CH_fullsize_{site.year - 4}{site.year}_allmonths_5Y.tif"))
    max_precip_10Y = rasterio.open(os.path.join(config.precip_10Y, f"Max_CHELSA_LV95_CH_fullsize_{site.year - 9}{site.year}_allmonths.tif"))

    stack_precip_5Y = rasterio.open(os.path.join(config.precip_5Y, f"Stack_CHELSA_LV95_CH_fullsize_{site.year - 4}{site.year}_allmonths_5Y.tif"))
    stack_precip_10Y = rasterio.open(os.path.join(config.precip_10Y, f"Stack_CHELSA_LV95_CH_fullsize_{site.year - 9}{site.year}_allmonths.tif"))

    # TODO why here 2009-2013 fixed?
    snow_days = rasterio.open(os.path.join(config.snow_days_dir, "CHELSA_LV95_CH_snow_days_mean_20092013.tif"))
    snow_cover_days = rasterio.open(os.path.join(config.snow_cover_days_dir, "CHELSA_LV95_CH_scd_mean_20092013.tif"))
    grow_season_length = rasterio.open(os.path.join(config.grow_season_length_dir, "CHELSA_LV95_CH_gsl_mean_20092013.tif"))
    frost_ch_freq = rasterio.open(os.path.join(config.frost_ch_freq_dir, "CHELSA_LV95_CH_fcf_mean_20092013.tif"))

    filled_elevation = elevation.read(1, masked=True).filled(0)

    roughness_data = ndimage.maximum_filter(filled_elevation, size=3) - ndimage.minimum_filter(filled_elevation, size=3)

    roughness_file = os.path.join(config.roughness, f"Roughness_2m_{site.name}.tif")
    if not os.path.exists(roughness_file):
        roughness = rasterio.open(
            roughness_file,
            'w',
            driver = 'GTiff',
            height = roughness_data.shape[0],
            width = roughness_data.shape[1],
            count = 1,
            dtype = roughness_data.dtype,
            crs = elevation.crs,
            transform = elevation.transform,
        )
        roughness.write(roughness_data, 1)
        roughness.close()
    roughness = rasterio.open(roughness_file)

    distance_to_roads = rasterio.open(os.path.join(config.distance_to_roads_dir, "CH_Distance_To_Roads_10m.tif"))
    distance_to_streams = rasterio.open(os.path.join(config.distance_to_streams_dir, "CH_Distance_To_Streams_10m.tif"))

    density_roads = rasterio.open(os.path.join(config.density_roads_dir, "CH_Density_Roads_500m.tif"))
    density_streams = rasterio.open(os.path.join(config.density_streams_dir, "CH_Density_Streams_500m.tif"))

    coord_list = list(zip(gdf_points['geometry'].x , gdf_points['geometry'].y))

    hmv = handle_missing_values
    gdf_points['elevation'] = [hmv(x[0]) for x in elevation.sample(coord_list)]
    gdf_points['slope'] = [hmv(x[0]) for x in slope.sample(coord_list)]
    gdf_points['aspect'] = [hmv(x[0]) for x in aspect.sample(coord_list)]
    gdf_points['twi'] = [hmv(x[0]) for x in twi.sample(coord_list)]
    gdf_points['flowacc'] = [hmv(x[0]) for x in flowacc.sample(coord_list)]
    gdf_points['flowdir'] = [hmv(x[0]) for x in flowdir.sample(coord_list)]
    gdf_points['curvature_plan'] = [hmv(x[0]) for x in curvature_plan.sample(coord_list)]
    gdf_points['curvature_profile'] = [hmv(x[0]) for x in curvature_profile.sample(coord_list)]
    gdf_points['max_precip_5Y'] = [hmv(x[0]) for x in max_precip_5Y.sample(coord_list)]
    gdf_points['max_precip_10Y'] = [hmv(x[0]) for x in max_precip_10Y.sample(coord_list)]
    gdf_points['stack_precip_5Y'] = [hmv(x[0]) for x in stack_precip_5Y.sample(coord_list)]
    gdf_points['stack_precip_10Y'] = [hmv(x[0]) for x in stack_precip_10Y.sample(coord_list)]
    gdf_points['snow_days'] = [hmv(x[0]) for x in snow_days.sample(coord_list)]
    gdf_points['snow_cover_days'] = [hmv(x[0]) for x in snow_cover_days.sample(coord_list)]
    gdf_points['grow_season_length'] = [hmv(x[0]) for x in grow_season_length.sample(coord_list)]
    gdf_points['frost_ch_freq'] = [hmv(x[0]) for x in frost_ch_freq.sample(coord_list)]
    gdf_points['distance_to_roads'] = [hmv(x[0]) for x in distance_to_roads.sample(coord_list)]
    gdf_points['distance_to_streams'] = [hmv(x[0]) for x in distance_to_streams.sample(coord_list)]
    gdf_points['density_roads'] = [hmv(x[0]) for x in density_roads.sample(coord_list)]
    gdf_points['density_streams'] = [hmv(x[0]) for x in density_streams.sample(coord_list)]

    gdf_points['roughness'] = [hmv(x[0]) for x in roughness.sample(coord_list)]

    lithology = gpd.read_file(os.path.join(config.lithology_dir, f"{site.name}_Lithology.shp"))
    gdf_points['lithology'] = gdf_points.sjoin(lithology, how="left", predicate='intersects')['LITHO']

    gesteinsklasse = gpd.read_file(os.path.join(config.gesteinsklasse_dir, f"{site.name}_Gesteinsklasse.shp"))
    gdf_points['gesteinsklasse'] = gdf_points.sjoin(gesteinsklasse, how="left", predicate='intersects')['GESTEINKL']

    gdf_points = gdf_points.rename(columns={'lslpts': 'landslide'})

    # Store data of site
    with open(os.path.join(output_dir, f'{site.name}_geo_df.pickle'), 'wb') as handle:
        pickle.dump(gdf_points, handle, protocol=pickle.HIGHEST_PROTOCOL)
