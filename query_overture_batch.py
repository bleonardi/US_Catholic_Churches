import duckdb
import pandas as pd
import json
import os

# Load the candidates
df = pd.read_csv('diocesan_top_10.csv')

con = duckdb.connect()
con.execute("INSTALL httpfs; LOAD httpfs;")
con.execute("INSTALL spatial; LOAD spatial;")
con.execute("SET s3_region='us-west-2';")
con.execute("SET s3_access_key_id=''; SET s3_secret_access_key='';")

url = "s3://overturemaps-us-west-2/release/2026-05-20.0/theme=base/type=infrastructure/*.parquet"

results = []

print(f"Total candidates to process: {len(df)}")

# Process in small chunks to avoid memory issues and provide progress
for i, row in df.iterrows():
    if i % 10 == 0:
        print(f"Processing candidate {i}/{len(df)}: {row['name']}...")
    
    lat = row['latitude']
    lon = row['longitude']
    
    query = f"""
    SELECT 
        SUM(ST_Area(geometry)) as total_area
    FROM read_parquet('{url}')
    WHERE class = 'parking'
      AND bbox.xmin > {lon - 0.003}
      AND bbox.xmax < {lon + 0.003}
      AND bbox.ymin > {lat - 0.003}
      AND bbox.ymax < {lat + 0.003}
    """
    try:
        res = con.execute(query).fetchone()
        area = res[0] if res[0] else 0
        results.append({
            "psuedo_id": row['psuedo_id'],
            "overture_parking_area_m2": round(float(area * 111139**2), 1)
        })
    except Exception as e:
        # print(f"Error querying {row['name']}: {e}")
        results.append({
            "psuedo_id": row['psuedo_id'],
            "overture_parking_area_m2": 0
        })

# Save results as CSV for easy joining in R/Quarto
pd.DataFrame(results).to_csv('overture_parking_results.csv', index=False)
print("Batch Overture processing complete. Results saved to overture_parking_results.csv")
