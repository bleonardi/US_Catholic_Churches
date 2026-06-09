import duckdb
import pandas as pd
import json

# Diocese of Cincinnati BBOX (from R script)
bbox = {
    "xmin": -84.79,
    "xmax": -83.38,
    "ymin": 38.74,
    "ymax": 40.66
}

con = duckdb.connect()
con.execute("INSTALL httpfs; LOAD httpfs;")
con.execute("INSTALL spatial; LOAD spatial;")
con.execute("SET s3_region='us-west-2';")

release_path = "s3://overturemaps-us-west-2/release/2026-05-20.0"

# 1. Load Churches from CSV
print("Loading churches...")
churches_df = pd.read_csv("national_church_data.csv")
cin_churches = churches_df[churches_df['diocese_name'] == 'Cincinnati'].dropna(subset=['latitude', 'longitude'])
cin_churches[['name', 'latitude', 'longitude', 'psuedo_id']].to_csv("cin_churches_coords.csv", index=False)

# 2. Query Overture Places (Interaction) for the entire BBOX
print("Querying Overture Places for Diocese BBOX...")
query_places = f"""
SELECT 
    names.primary AS name,
    categories.primary AS category,
    ST_X(geometry) AS lon,
    ST_Y(geometry) AS lat
FROM read_parquet('{release_path}/theme=places/type=place/*.parquet')
WHERE bbox.xmin > {bbox['xmin']}
  AND bbox.xmax < {bbox['xmax']}
  AND bbox.ymin > {bbox['ymin']}
  AND bbox.ymax < {bbox['ymax']}
"""
places_df = con.execute(query_places).df()

# Filter relevant interaction categories
relevant_categories = ['park', 'public_space', 'tourist_attraction', 'restaurant', 'cafe', 'shopping_center']
places_df = places_df[
    (places_df['category'].str.contains('park', case=False, na=False) & ~places_df['category'].str.contains('parking', case=False, na=False)) |
    (places_df['category'].isin(relevant_categories))
]
places_df.to_csv("diocese_places.csv", index=False)

# 3. Query Overture Transportation (Safety) for the entire BBOX
# NOTE: This might be large, we'll focus on 'pedestrian' and 'footway' classes to keep it manageable
print("Querying Overture Transportation (pedestrian/footway) for Diocese BBOX...")
query_trans = f"""
SELECT 
    class,
    ST_AsText(geometry) as wkt
FROM read_parquet('{release_path}/theme=transportation/type=segment/*.parquet')
WHERE bbox.xmin > {bbox['xmin']}
  AND bbox.xmax < {bbox['xmax']}
  AND bbox.ymin > {bbox['ymin']}
  AND bbox.ymax < {bbox['ymax']}
  AND class IN ('pedestrian', 'footway', 'path', 'residential', 'living_street')
"""
trans_df = con.execute(query_trans).df()
trans_df.to_csv("diocese_segments.csv", index=False)

print(f"Data saved. Churches: {len(cin_churches)}, Places: {len(places_df)}, Segments: {len(trans_df)}")
