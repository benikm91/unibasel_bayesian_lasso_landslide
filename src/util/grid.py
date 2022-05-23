import numpy as np
import geopandas as gpd
from shapely.geometry import Polygon

def generate_gird(length: int, wide: int, crs, xmin, ymin, xmax, ymax):
    # THANKS TO https://gis.stackexchange.com/questions/269243/creating-polygon-grid-using-geopandas
    cols = list(np.arange(xmin, xmax + wide, wide))
    rows = list(np.arange(ymin, ymax + length, length))

    polygons = []
    for x in cols[:-1]:
        for y in rows[:-1]:
            polygons.append(Polygon([(x, y), (x + wide, y), (x + wide, y + length), (x, y + length)]))

    return gpd.GeoDataFrame({'geometry': polygons}) \
        .set_crs(crs)