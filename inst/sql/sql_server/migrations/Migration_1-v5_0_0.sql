-- Database migrations for version 5.0.0
-- This migration updates the schema:
 -- 1. Deprecates the outcome_rate_ adjusted_rate, stable, and p fields from the time_trend  table (no change to the data).
 -- 2. Adds the ratio and adjusted_ratio fields to the time_trend  table

ALTER TABLE @database_schema.@table_prefixsccs_time_trend ADD ratio FLOAT, adjusted_ratio FLOAT;
