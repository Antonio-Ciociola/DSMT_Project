-- Migration script to rename initial_wait_time to starting_duration
ALTER TABLE auctions CHANGE COLUMN initial_wait_time starting_duration INT NOT NULL;
