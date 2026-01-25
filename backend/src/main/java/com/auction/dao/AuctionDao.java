package com.auction.dao;

import com.auction.model.Auction;
import com.auction.model.DatabaseConnection;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class AuctionDao {

    // Fetch upcoming auctions along with owner usernames
    public List<Auction> findUpcomingWithOwner() throws SQLException {

        // SQL query to join auctions with users to get owner usernames
        String sql = "SELECT a.id, a.user_id, a.title, a.description, a.starting_price, a.min_bid_increment, " +
                     "a.countdown_timer, a.start_date, u.username " +
                     "FROM auctions a JOIN users u ON a.user_id = u.id " +
                     "WHERE a.start_date >= NOW() ORDER BY a.start_date ASC";
        
        // Getting DB connection, executing the query and mapping results to 
        // Auction objects
        // Using try-with-resources because even if exceptions occur, resources 
        // will be closed
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql);
            ResultSet rs = pstmt.executeQuery()
        ) {
            List<Auction> list = new ArrayList<>();
            while (rs.next()) {
                list.add(mapAuction(rs, true));
            }
            return list;
        }
    }

    // Fetch upcoming auctions created by a specific user
    public List<Auction> findUpcomingByUser(int userId) throws SQLException {

        // SQL query to get auctions for a user that haven't started yet
        String sql = "SELECT id, user_id, title, description, starting_price, min_bid_increment, " +
                     "countdown_timer, start_date FROM auctions " +
                     "WHERE user_id = ? AND start_date > NOW() ORDER BY start_date ASC";
        
        // Getting DB connection, preparing the statement with userId, 
        // executing the query and mapping results to Auction objects
        // Nested try-with-resources to ensure all resources are closed properly
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {

            // Set userId parameter, it can't be done inside the try-with-resources
            pstmt.setInt(1, userId);

            // Execute query and map results
            try (ResultSet rs = pstmt.executeQuery()) {
                List<Auction> list = new ArrayList<>();
                while (rs.next()) {
                    list.add(mapAuction(rs, false));
                }
                return list;
            }
        }
    }

    // Create a new auction in the database
    public void createAuction(Auction auction) throws SQLException {

        // SQL insert statement to add a new auction
        String sql = "INSERT INTO auctions (user_id, title, description, starting_price, min_bid_increment, countdown_timer, start_date) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?)";

        // Getting DB connection, preparing the statement with auction details
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setInt(1, auction.getUserId());
            pstmt.setString(2, auction.getTitle());
            pstmt.setString(3, auction.getDescription() != null ? auction.getDescription() : "");
            pstmt.setDouble(4, auction.getStartingPrice());
            pstmt.setDouble(5, auction.getMinBidIncrement());
            pstmt.setInt(6, auction.getCountdownTimer());
            pstmt.setTimestamp(7, Timestamp.valueOf(auction.getStartDate()));
            pstmt.executeUpdate();
        }
    }

    // Delete an upcoming auction owned by a specific user
    public boolean deleteUpcomingOwnedAuction(int auctionId, int userId) throws SQLException {
        
        // SQL statements to verify ownership and delete the auction
        String verifySql = "SELECT id FROM auctions WHERE id = ? AND user_id = ? AND start_date > NOW()";
        String deleteSql = "DELETE FROM auctions WHERE id = ? AND user_id = ?";

        // Getting DB connection, verifying ownership, and deleting the auction
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement verifyStmt = conn.prepareStatement(verifySql)
        ) {

            // Verify that the auction exists, is owned by the user, and hasn't started yet
            verifyStmt.setInt(1, auctionId);
            verifyStmt.setInt(2, userId);
            try (ResultSet rs = verifyStmt.executeQuery()) {
                if (!rs.next()) {
                    return false;
                }
            }

            // Proceed to delete the auction
            try (PreparedStatement deleteStmt = conn.prepareStatement(deleteSql)) {
                deleteStmt.setInt(1, auctionId);
                deleteStmt.setInt(2, userId);
                deleteStmt.executeUpdate();
                return true;
            }
        }
    }

    // Fetch an auction by its ID
    public Optional<Auction> findById(int auctionId) throws SQLException {
        
        // SQL query to get auction details by ID
        String sql = "SELECT id, user_id, title, description, starting_price, min_bid_increment, countdown_timer, start_date " +
                     "FROM auctions WHERE id = ?";

        // Getting DB connection, preparing the statement with auctionId, 
        // executing the query and mapping result to Auction object
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setInt(1, auctionId);

            // Using Optional to handle the case where the auction may not exist
            try (ResultSet rs = pstmt.executeQuery()) {
                if (rs.next()) {
                    return Optional.of(mapAuction(rs, false));
                }
                return Optional.empty();
            }
        }
    }

    // Helper method to map a ResultSet row to an Auction object
    private Auction mapAuction(ResultSet rs, boolean includeOwner) throws SQLException {
        Auction auction = new Auction();
        auction.setId(rs.getInt("id"));
        auction.setUserId(rs.getInt("user_id"));
        auction.setTitle(rs.getString("title"));
        auction.setDescription(rs.getString("description"));
        auction.setStartingPrice(rs.getDouble("starting_price"));
        auction.setMinBidIncrement(rs.getDouble("min_bid_increment"));
        auction.setCountdownTimer(rs.getInt("countdown_timer"));
        Timestamp ts = rs.getTimestamp("start_date");
        auction.setStartDate(ts != null ? ts.toLocalDateTime() : null);
        if (includeOwner) {
            auction.setOwnerUsername(rs.getString("username"));
        }
        return auction;
    }
}
