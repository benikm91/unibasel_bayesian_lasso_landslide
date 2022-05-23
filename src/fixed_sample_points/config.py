import os

points_dir = os.path.join(os.path.dirname(__file__), "data", "points")

numeric_cols = [
    'elevation',
    'slope',
    'curvature_plan',
    'curvature_profile',
    'roughness',
    'flowacc',
    'twi',  # Topographic wetness index
    'distance_to_roads',
    'distance_to_streams',
    'density_roads',
    'density_streams',
    'flowdir',
    'max_precip_5Y',
    'max_precip_10Y',
    'stack_precip_5Y',
    'stack_precip_10Y',
    'snow_days',
    'snow_cover_days',
    'grow_season_length',
    'frost_ch_freq',
]
categorical_cols = [
    'gesteinsklasse',
    'aspect_factor'
]
features = numeric_cols + categorical_cols