-- Migration script to add auction_id_bidding column to users table
-- Run this on existing databases to add the new column

-- Add auction_id_bidding column to users table
ALTER TABLE users 
ADD COLUMN auction_id_bidding INT NULL,
ADD CONSTRAINT fk_users_auction_bidding 
    FOREIGN KEY (auction_id_bidding) 
    REFERENCES auctions(id) 
    ON DELETE SET NULL;

-- Log success
SELECT 'Migration completed: auction_id_bidding column added to users table' AS status;
