import os
from dataclasses import dataclass
from typing import Dict, List

from util.site import Site


@dataclass
class Config:
    sites: List[Site]
    distance_to_roads_dir: str
    distance_to_streams_dir: str
    density_roads_dir: str
    density_streams_dir: str
    snow_days_dir: str
    snow_cover_days_dir: str
    grow_season_length_dir: str
    frost_ch_freq_dir: str
    lithology_dir: str
    gesteinsklasse_dir: str
    precip_10Y: str
    precip_5Y: str
    elevation: str
    roughness: str
    slope: str
    aspect: str
    topo_wetness_index: str
    flow_accumulation: str
    flow_direction: str
    curvature: str

    @staticmethod
    def default_sites() -> List[Site]:
        return [
          Site("Arosa", 2014),
          Site("Baulmes", 2014),
          Site("Chrauchtal", 2014),
          Site("Hornbach", 2015),
          Site("Rappetal", 2015),
          Site("Turbach", 2013),
          Site("Urseren", 2013),
          Site("Val_Cluozza", 2015),
          Site("Val_D_Entremont", 2013),
          Site("Val_Piora", 2015)
        ]

    @staticmethod
    def default() -> 'Config':
        data_dir = "/Users/beni/Documents/BayianLasso/data/data"
        explanatory_variables_dir = os.path.join(data_dir, "Explanatory_Variables_Datasets")
        return Config(
            Config.default_sites(),
            # explanatory_variables
            os.path.join(explanatory_variables_dir, "Distance_To_Roads"),
            os.path.join(explanatory_variables_dir, "Distance_To_Streams"),
            os.path.join(explanatory_variables_dir, "Density_Roads"),
            os.path.join(explanatory_variables_dir, "Density_Streams"),
            os.path.join(explanatory_variables_dir, "Snow_Days"),
            os.path.join(explanatory_variables_dir, "Snow_Cover_Days"),
            os.path.join(explanatory_variables_dir, "Growing_Season_Length"),
            os.path.join(explanatory_variables_dir, "Frost_Change_Frequency"),
            os.path.join(explanatory_variables_dir, "Geology_Lithologie"),
            os.path.join(explanatory_variables_dir, "Geology_Gesteinsklasse"),
            os.path.join(explanatory_variables_dir, "Precip_10Y"),
            os.path.join(explanatory_variables_dir, "Precip_5Y"),
            #
            os.path.join(explanatory_variables_dir, "Elevation"),
            os.path.join(explanatory_variables_dir, "Roughness"),
            os.path.join(explanatory_variables_dir, "Slope"),
            os.path.join(explanatory_variables_dir, "Aspect"),
            os.path.join(explanatory_variables_dir, "Topo_Wetness_Index"),
            os.path.join(explanatory_variables_dir, "Flow_Accumulation"),
            os.path.join(explanatory_variables_dir, "Flow_Direction"),
            os.path.join(explanatory_variables_dir, "Curvature"),
        )
