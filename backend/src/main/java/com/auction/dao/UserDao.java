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
        String sql = "SELECT id, username, password, balance FROM users WHERE username = ?";
        
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

    // Fetch a user by their ID
    public Optional<User> findById(int userId) throws SQLException {

        // SQL query to get user details by ID
        String sql = "SELECT id, username, password, balance FROM users WHERE id = ?";
        
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

    // Helper method to map a ResultSet row to a User object
    private User mapUser(ResultSet rs) throws SQLException {
        User user = new User();
        user.setId(rs.getInt("id"));
        user.setUsername(rs.getString("username"));
        user.setPassword(rs.getString("password"));
        user.setBalance(rs.getBigDecimal("balance"));
        return user;
    }
}
