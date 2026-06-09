import duckdb
import json
import pandas as pd

# Cathedral Basilica of St. Peter in Chains, Cincinnati
loc = {"name": "Cathedral Basilica of St. Peter in Chains", "lat": 39.1035, "lon": -84.5195}

con = duckdb.connect()
con.execute("INSTALL httpfs; LOAD httpfs;")
con.execute("INSTALL spatial; LOAD spatial;")
con.execute("SET s3_region='us-west-2';")

release_path = "s3://overturemaps-us-west-2/release/2026-05-20.0"

# 1. Query Places (Interaction) - Fetch all in bbox first to avoid complex S3 filters
query_places = f"""
SELECT 
    names.primary AS name,
    categories.primary AS category,
    ST_X(geometry) AS lon,
    ST_Y(geometry) AS lat
FROM read_parquet('{release_path}/theme=places/type=place/*.parquet')
WHERE bbox.xmin > {loc['lon'] - 0.01}
  AND bbox.xmax < {loc['lon'] + 0.01}
  AND bbox.ymin > {loc['lat'] - 0.01}
  AND bbox.ymax < {loc['lat'] + 0.01}
"""

print("Querying Overture Places (all in bbox)...")
places_df = con.execute(query_places).df()

# Filter in Pandas
relevant_categories = ['park', 'public_space', 'tourist_attraction', 'restaurant', 'cafe', 'shopping_center']
places_df = places_df[
    (places_df['category'].str.contains('park', case=False, na=False) & ~places_df['category'].str.contains('parking', case=False, na=False)) |
    (places_df['category'].isin(relevant_categories))
]

places_df.to_csv("procession_places.csv", index=False)

# 2. Query Transportation (Safety/Sidewalks)
trans_url = f"{release_path}/theme=transportation/type=segment/*.parquet"
query_trans = f"""
SELECT 
    subtype,
    class,
    ST_AsText(geometry) as wkt
FROM read_parquet('{trans_url}')
WHERE bbox.xmin > {loc['lon'] - 0.01}
  AND bbox.xmax < {loc['lon'] + 0.01}
  AND bbox.ymin > {loc['lat'] - 0.01}
  AND bbox.ymax < {loc['lat'] + 0.01}
"""

print("Querying Overture Transportation...")
trans_df = con.execute(query_trans).df()
trans_df.to_csv("procession_segments.csv", index=False)

print(f"Data saved. Places: {len(places_df)}, Segments: {len(trans_df)}")
