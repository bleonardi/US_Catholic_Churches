import duckdb
import json
import os

locations = [
    {"name": "Gesu Chapel", "lat": 37.8776, "lon": -122.2585},
    {"name": "Columbus Law", "lat": 38.9363, "lon": -76.9966},
    {"name": "St. Patrick", "lat": 47.6442, "lon": -122.3212},
    {"name": "OLMC", "lat": 39.9355, "lon": -75.1225},
    {"name": "Holy Name", "lat": 39.9528, "lon": -75.1183}
]

con = duckdb.connect()
con.execute("INSTALL httpfs; LOAD httpfs;")
con.execute("INSTALL spatial; LOAD spatial;")
con.execute("SET s3_region='us-west-2';")
con.execute("SET s3_access_key_id=''; SET s3_secret_access_key='';")

# Correct path for Overture parking polygons
url = "s3://overturemaps-us-west-2/release/2026-05-20.0/theme=base/type=infrastructure/*.parquet"

results = []

for loc in locations:
    print(f"Querying Overture Infrastructure for {loc['name']}...")
    # Using spatial pruning with bbox
    query = f"""
    SELECT 
        SUM(ST_Area(geometry)) as total_area
    FROM read_parquet('{url}')
    WHERE class = 'parking'
      AND bbox.xmin > {loc['lon'] - 0.005}
      AND bbox.xmax < {loc['lon'] + 0.005}
      AND bbox.ymin > {loc['lat'] - 0.005}
      AND bbox.ymax < {loc['lat'] + 0.005}
    """
    try:
        res = con.execute(query).fetchone()
        area = res[0] if res[0] else 0
        results.append({
            "name": loc['name'], 
            "overture_parking_area_m2": float(area * 111139**2),
            "source": "Overture Maps (Infrastructure)"
        })
    except Exception as e:
        print(f"Error querying {loc['name']}: {e}")
        results.append({"name": loc['name'], "overture_parking_area_m2": 0, "error": str(e)})

with open("overture_results.json", "w") as f:
    json.dump(results, f, indent=2)

print(json.dumps(results, indent=2))
