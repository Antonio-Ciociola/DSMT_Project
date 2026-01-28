package com.auction.service;

import com.auction.dao.UserDao;
import com.auction.model.User;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.Optional;

// Service class for managing user-related operations
public class UserService {

    // DAO instance to interact with user data
    private final UserDao userDao;

    // Constructor initializing the UserDao
    public UserService() {
        this.userDao = new UserDao();
    }

    // Authenticate user by username and password
    public Optional<User> authenticate(String username, String password) throws SQLException {
        
        // Fetch user by username, handling not found case with Optional
        Optional<User> userOpt = userDao.findByUsername(username);

        // If user exists then verify password (in a real application, use hashed 
        // passwords and a secure comparison method), if password matches return user
        if (userOpt.isPresent()) {
            User user = userOpt.get();
            if (user.getPassword() != null && user.getPassword().equals(password)) {
                return userOpt;
            }
        }

        // Authentication failed
        return Optional.empty();
    }

    // Register a new user with username and password
    public void register(String username, String password) throws SQLException {
        userDao.createUser(username, password);
    }

    // Get user balance by user ID
    public BigDecimal getBalance(int userId) throws SQLException {
        return userDao.getBalance(userId);
    }

    // Add amount to user balance
    public void addToBalance(int userId, BigDecimal amount) throws SQLException {
        userDao.addToBalance(userId, amount);
    }
    
    // Get user by ID
    public Optional<User> getUserById(int userId) throws SQLException {
        return userDao.findById(userId);
    }
    
    // Set the auction the user is currently bidding in
    public void setAuctionIdBidding(int userId, Integer auctionId) throws SQLException {
        userDao.setAuctionIdBidding(userId, auctionId);
    }
    
    // Clear auction_id_bidding for all users in a specific auction
    public void clearAuctionIdBiddingForAuction(int auctionId) throws SQLException {
        userDao.clearAuctionIdBiddingForAuction(auctionId);
    }
}
