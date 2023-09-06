-- Database migrations for version 5.0.0
-- This migration updates the schema:
 -- 1. Deprecates the outcome_rate_ adjusted_rate, stable, and p fields from the time_trend  table (no change to the data).
 -- 2. Adds the ratio and adjusted_ratio fields to the time_trend  table

{DEFAULT @package_version = package_version}
{DEFAULT @migration = migration}
{DEFAULT @table_prefix = ''}

-- Create table indicating version number of ddl
DROP TABLE IF EXISTS @database_schema.@table_prefix@package_version;

--HINT DISTRIBUTE ON RANDOM
CREATE TABLE @database_schema.@table_prefix@package_version (
    version_number VARCHAR(50) PRIMARY KEY
);

ALTER TABLE @database_schema.@table_prefixsccs_time_trend ADD ratio FLOAT, adjusted_ratio FLOAT;
