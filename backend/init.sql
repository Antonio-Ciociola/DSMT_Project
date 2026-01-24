-- Create users table
CREATE TABLE IF NOT EXISTS users (
    id INT PRIMARY KEY AUTO_INCREMENT,
    username VARCHAR(50) UNIQUE NOT NULL,
    password VARCHAR(100) NOT NULL,
    balance DECIMAL(10, 2) DEFAULT 0
);

-- Create auctions table
CREATE TABLE IF NOT EXISTS auctions (
    id INT PRIMARY KEY AUTO_INCREMENT,
    user_id INT NOT NULL,
    title VARCHAR(200) NOT NULL,
    description TEXT,
    starting_price DECIMAL(10, 2) NOT NULL,
    min_bid_increment DECIMAL(10, 2) NOT NULL,
    countdown_timer INT NOT NULL,
    start_date DATETIME NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Create sample data
INSERT INTO users (username, password, balance) VALUES 
('user', 'password', 1000.00),
('admin', 'admin123', 5000.00);
