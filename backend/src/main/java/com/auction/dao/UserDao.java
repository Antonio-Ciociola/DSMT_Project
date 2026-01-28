package com.auction.dao;

import com.auction.model.DatabaseConnection;
import com.auction.model.User;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Optional;

// Data Access Object for User-related database operations
public class UserDao {

    // Fetch a user by their username
    public Optional<User> findByUsername(String username) throws SQLException {
        
        // SQL query to get user details by username
        String sql = "SELECT id, username, password, balance, auction_id_bidding FROM users WHERE username = ?";
        
        // Getting DB connection, preparing the statement with username, 
        // executing the query and mapping result to User object
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setString(1, username);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    User user = mapUser(rs);
                    return Optional.of(user);
                }
                return Optional.empty();
            }
        }
    }

    // Get user ID by username (returns 0 if not found)
    public int getUserIdByUsername(String username) throws SQLException {
        Optional<User> userOpt = findByUsername(username);
        return userOpt.map(User::getId).orElse(0);
    }

    // Fetch a user by their ID
    public Optional<User> findById(int userId) throws SQLException {

        // SQL query to get user details by ID
        String sql = "SELECT id, username, password, balance, auction_id_bidding FROM users WHERE id = ?";
        
        // Getting DB connection, preparing the statement with userId, 
        // executing the query and mapping result to User object
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setInt(1, userId);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    User user = mapUser(rs);
                    return Optional.of(user);
                }
                return Optional.empty();
            }
        }
    }

    // Create a new user in the database
    public void createUser(String username, String password) throws SQLException {
        
        // SQL insert statement to add a new user
        String sql = "INSERT INTO users (username, password, balance) VALUES (?, ?, 0.0)";
        
        // Getting DB connection, preparing the statement with username and password
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
    ) {
            pstmt.setString(1, username);
            pstmt.setString(2, password);
            pstmt.executeUpdate();
        }
    }

    // Get the balance of a user by their ID
    public BigDecimal getBalance(int userId) throws SQLException {

        // SQL query to get user balance by ID
        String sql = "SELECT balance FROM users WHERE id = ?";

        // Getting DB connection, preparing the statement with userId, 
        // executing the query and retrieving the balance
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setInt(1, userId);
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    BigDecimal balance = rs.getBigDecimal("balance");
                    return balance != null ? balance : BigDecimal.ZERO;
                }
                throw new SQLException("User not found for id " + userId);
            }
        }
    }

    // Add an amount to the user's balance
    public void addToBalance(int userId, BigDecimal amount) throws SQLException {
        
        // SQL update statement to increase user balance
        String sql = "UPDATE users SET balance = balance + ? WHERE id = ?";

        // Getting DB connection, preparing the statement with amount and userId
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setBigDecimal(1, amount);
            pstmt.setInt(2, userId);
            int updated = pstmt.executeUpdate();
            if (updated == 0) {
                throw new SQLException("User not found for id " + userId);
            }
        }
    }

    // Transfer balance from one user to another (winner pays, seller receives)
    public void transferBalance(int fromUserId, int toUserId, BigDecimal amount) throws SQLException {
        // Two separate updates wrapped in a transaction to avoid multi-statement issues
        String debitSql = "UPDATE users SET balance = balance - ? WHERE id = ?";
        String creditSql = "UPDATE users SET balance = balance + ? WHERE id = ?";

        try (
            Connection conn = DatabaseConnection.getConnection()
        ) {

            // Start transaction
            boolean originalAutoCommit = conn.getAutoCommit();
            conn.setAutoCommit(false);

            // Prepare and execute debit and credit statements
            try (
                PreparedStatement debitStmt = conn.prepareStatement(debitSql);
                PreparedStatement creditStmt = conn.prepareStatement(creditSql)
            ) {
                // Debit payer
                debitStmt.setBigDecimal(1, amount);
                debitStmt.setInt(2, fromUserId);
                int debitUpdated = debitStmt.executeUpdate();
                if (debitUpdated == 0) {
                    throw new SQLException("User not found for id " + fromUserId);
                }

                // Credit seller
                creditStmt.setBigDecimal(1, amount);
                creditStmt.setInt(2, toUserId);
                int creditUpdated = creditStmt.executeUpdate();
                if (creditUpdated == 0) {
                    throw new SQLException("User not found for id " + toUserId);
                }

                // Commit transaction
                conn.commit();
            } 
            // Rollback transaction on error
            catch (SQLException ex) {
                conn.rollback();
                throw ex;
            } 
            // Restore original auto-commit setting
            finally {
                conn.setAutoCommit(originalAutoCommit);
            }
        }
    }

    // Set the auction the user is currently bidding in
    public void setAuctionIdBidding(int userId, Integer auctionId) throws SQLException {
        String sql = "UPDATE users SET auction_id_bidding = ? WHERE id = ?";
        
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            if (auctionId == null) {
                pstmt.setNull(1, java.sql.Types.INTEGER);
            } else {
                pstmt.setInt(1, auctionId);
            }
            pstmt.setInt(2, userId);
            pstmt.executeUpdate();
        }
    }
    
    // Clear auction_id_bidding for all users in a specific auction
    public void clearAuctionIdBiddingForAuction(int auctionId) throws SQLException {
        String sql = "UPDATE users SET auction_id_bidding = NULL WHERE auction_id_bidding = ?";
        
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setInt(1, auctionId);
            int updated = pstmt.executeUpdate();
            System.out.format("[DAO] Cleared auction_id_bidding for %d users in auction %d%n", updated, auctionId);
        }
    }

    // Helper method to map a ResultSet row to a User object
    private User mapUser(ResultSet rs) throws SQLException {
        User user = new User();
        user.setId(rs.getInt("id"));
        user.setUsername(rs.getString("username"));
        user.setPassword(rs.getString("password"));
        user.setBalance(rs.getBigDecimal("balance"));
        
        // Handle nullable auction_id_bidding
        int auctionIdBidding = rs.getInt("auction_id_bidding");
        if (!rs.wasNull()) {
            user.setAuctionIdBidding(auctionIdBidding);
        }
        
        return user;
    }
}
