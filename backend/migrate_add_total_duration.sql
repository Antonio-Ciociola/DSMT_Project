-- Migration script to add total_duration column to auctions table
-- Run this on existing databases that don't have the total_duration column

ALTER TABLE auctions 
ADD COLUMN total_duration INT NULL 
AFTER starting_duration;

-- For existing finished auctions, set total_duration to starting_duration as a fallback
UPDATE auctions 
SET total_duration = starting_duration 
WHERE status = 'finished' AND total_duration IS NULL;
