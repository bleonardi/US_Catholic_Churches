import duckdb
import pandas as pd
import os

# Load the candidates
df = pd.read_csv('diocesan_top_10.csv')

con = duckdb.connect()
con.execute("INSTALL httpfs; LOAD httpfs;")
con.execute("INSTALL spatial; LOAD spatial;")
con.execute("SET s3_region='us-west-2';")
con.execute("SET s3_access_key_id=''; SET s3_secret_access_key='';")

url = "s3://overturemaps-us-west-2/release/2026-05-20.0/theme=base/type=infrastructure/*.parquet"

# Create a temporary table of our candidates in DuckDB
con.execute("CREATE TABLE candidates AS SELECT * FROM df")

print("Starting high-speed Overture spatial join...")

# The "Turbo" Query: Join our candidates with Overture in one pass
# We use ST_Intersects on a small buffer (0.002 deg ~ 200m) 
# and filter by Overture's internal bbox first for performance.
query = f"""
CREATE TABLE results AS
SELECT 
    c.psuedo_id,
    SUM(ST_Area(o.geometry)) as total_area
FROM candidates c, read_parquet('{url}') o
WHERE o.class = 'parking'
  AND o.bbox.xmin > c.longitude - 0.005
  AND o.bbox.xmax < c.longitude + 0.005
  AND o.bbox.ymin > c.latitude - 0.005
  AND o.bbox.ymax < c.latitude + 0.005
  AND ST_Intersects(o.geometry, ST_Buffer(ST_Point(c.longitude, c.latitude), 0.002))
GROUP BY c.psuedo_id
"""

try:
    con.execute(query)
    # Fetch and calculate m2
    output = con.execute("SELECT psuedo_id, total_area * (111139*111139) as overture_parking_area_m2 FROM results").df()
    output.to_csv('overture_parking_results.csv', index=False)
    print(f"Success! Processed {len(output)} locations with high-speed join.")
except Exception as e:
    print(f"Turbo query failed: {e}")
    # Fallback plan is already being replaced, but let's log the error
