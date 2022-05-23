def angle_to_direction(degree):
    if degree <= 22.5 or degree > 337.5:
        return "N"
    if 22.5 < degree <= 67.5:
        return "NE"
    if 67.5 < degree <= 112.5:
        return "E"
    if 112.5 < degree <= 157.5:
        return "SE"
    if 157.5 < degree <= 202.5:
        return "S"
    if 202.5 < degree <= 247.5:
        return "SW"
    if 247.5 < degree <= 292.5:
        return "W"
    if 292.5 < degree <= 337.5:
        return "NW"
