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

    // Fetch all auctions along with owner usernames and winner info
    public List<Auction> findAllWithOwner() throws SQLException {

        // SQL query to join auctions with users to get owner and winner usernames
        String sql = "SELECT a.id, a.user_id, a.title, a.description, a.starting_price, a.min_bid_increment, " +
                     "a.start_date, a.status, a.winner_user_id, a.final_price, " +
                     "a.starting_duration, a.total_duration, a.bid_time_increment, a.websocket_url, u.username as owner_username, " +
                     "w.username as winner_username " +
                     "FROM auctions a " +
                     "JOIN users u ON a.user_id = u.id " +
                     "LEFT JOIN users w ON a.winner_user_id = w.id " +
                     "ORDER BY a.start_date DESC";
        
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
                     "start_date, status, winner_user_id, final_price, " +
                     "starting_duration, bid_time_increment, total_duration, websocket_url FROM auctions " +
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
    public int createAuction(Auction auction) throws SQLException {

        // SQL insert statement to add a new auction
        String sql = "INSERT INTO auctions (user_id, title, description, starting_price, min_bid_increment, " +
                     "start_date, status, starting_duration, bid_time_increment) " +
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";

        // Getting DB connection, preparing the statement with auction details
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql, PreparedStatement.RETURN_GENERATED_KEYS)
        ) {
            pstmt.setInt(1, auction.getUserId());
            pstmt.setString(2, auction.getTitle());
            pstmt.setString(3, auction.getDescription() != null ? auction.getDescription() : "");
            pstmt.setDouble(4, auction.getStartingPrice());
            pstmt.setDouble(5, auction.getMinBidIncrement());
            pstmt.setTimestamp(6, Timestamp.valueOf(auction.getStartDate()));
            pstmt.setString(7, auction.getStatus() != null ? auction.getStatus() : "not_started");
            pstmt.setInt(8, auction.getStartingDuration());
            pstmt.setInt(9, auction.getBidTimeIncrement());
            pstmt.executeUpdate();
            
            // Get the generated auction ID
            try (ResultSet rs = pstmt.getGeneratedKeys()) {
                if (rs.next()) {
                    return rs.getInt(1);
                } else {
                    throw new SQLException("Failed to retrieve generated auction ID");
                }
            }
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
        String sql = "SELECT id, user_id, title, description, starting_price, min_bid_increment, " +
                     "start_date, status, winner_user_id, final_price, starting_duration, bid_time_increment, websocket_url " +
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

    // Finish an auction by setting status, winner, and final price
    public void finishAuction(int auctionId, int winnerUserId, double finalPrice, int totalDuration) throws SQLException {
        
        System.out.format("[DAO] finishAuction called - auctionId: %d, totalDuration: %d%n", auctionId, totalDuration);
        
        try (Connection conn = DatabaseConnection.getConnection()) {
            // Try to update with total_duration first
            String sql = "UPDATE auctions SET status = ?, winner_user_id = ?, final_price = ?, total_duration = ? WHERE id = ?";
            
            try (PreparedStatement pstmt = conn.prepareStatement(sql)) {
                pstmt.setString(1, "finished");
                pstmt.setInt(2, winnerUserId);
                pstmt.setDouble(3, finalPrice);
                pstmt.setInt(4, totalDuration);
                pstmt.setInt(5, auctionId);
                pstmt.executeUpdate();
                System.out.format("[DAO] Successfully updated auction %d with total_duration = %d%n", auctionId, totalDuration);
            } catch (SQLException e) {
                System.out.format("[DAO] Failed to update with total_duration, error: %s%n", e.getMessage());
                // If total_duration column doesn't exist, try without it
                if (e.getMessage().contains("total_duration")) {
                    String fallbackSql = "UPDATE auctions SET status = ?, winner_user_id = ?, final_price = ? WHERE id = ?";
                    try (PreparedStatement pstmt = conn.prepareStatement(fallbackSql)) {
                        pstmt.setString(1, "finished");
                        pstmt.setInt(2, winnerUserId);
                        pstmt.setDouble(3, finalPrice);
                        pstmt.setInt(4, auctionId);
                        pstmt.executeUpdate();
                    }
                } else {
                    throw e;
                }
            }
        }
    }

    public void finishAuctionWithNoWinner(int auctionId, int totalDuration) throws SQLException {
        
        try (Connection conn = DatabaseConnection.getConnection()) {
            // Try to update with total_duration first
            String sql = "UPDATE auctions SET status = ?, winner_user_id = NULL, final_price = NULL, total_duration = ? WHERE id = ?";
            
            try (PreparedStatement pstmt = conn.prepareStatement(sql)) {
                pstmt.setString(1, "finished");
                pstmt.setInt(2, totalDuration);
                pstmt.setInt(3, auctionId);
                pstmt.executeUpdate();
            } catch (SQLException e) {
                // If total_duration column doesn't exist, try without it
                if (e.getMessage().contains("total_duration")) {
                    String fallbackSql = "UPDATE auctions SET status = ?, winner_user_id = NULL, final_price = NULL WHERE id = ?";
                    try (PreparedStatement pstmt = conn.prepareStatement(fallbackSql)) {
                        pstmt.setString(1, "finished");
                        pstmt.setInt(2, auctionId);
                        pstmt.executeUpdate();
                    }
                } else {
                    throw e;
                }
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
        Timestamp ts = rs.getTimestamp("start_date");
        auction.setStartDate(ts != null ? ts.toLocalDateTime() : null);
        auction.setStatus(rs.getString("status"));
        auction.setStartingDuration(rs.getInt("starting_duration"));
        
        // Handle total_duration - may not exist in older database schemas
        try {
            int totalDuration = rs.getInt("total_duration");
            boolean wasNull = rs.wasNull();
            int auctionId = rs.getInt("id");
            System.out.format("[DAO] Read total_duration for auction %d: value=%d, wasNull=%b%n", 
                             auctionId, totalDuration, wasNull);
            
            if (!wasNull && totalDuration > 0) {
                auction.setTotalDuration(totalDuration);
                System.out.format("[DAO] Set totalDuration to %d for auction %d%n", totalDuration, auctionId);
            } else {
                // NULL or 0, use starting_duration as fallback
                int fallback = rs.getInt("starting_duration");
                auction.setTotalDuration(fallback);
                System.out.format("[DAO] Using starting_duration fallback (%d) for auction %d%n", fallback, auctionId);
            }
        } catch (SQLException e) {
            // Column doesn't exist, use starting_duration as fallback
            int fallback = rs.getInt("starting_duration");
            auction.setTotalDuration(fallback);
            System.out.format("[DAO] total_duration column doesn't exist, using fallback (%d)%n", fallback);
        }
        
        auction.setBidTimeIncrement(rs.getInt("bid_time_increment"));
        
        // Handle websocket_url with try-catch in case column doesn't exist
        try {
            auction.setWebsocketUrl(rs.getString("websocket_url"));
        } catch (SQLException e) {
            // Column doesn't exist or is null, set null
            auction.setWebsocketUrl(null);
        }
        
        // Handle nullable winner fields
        int winnerUserId = rs.getInt("winner_user_id");
        if (!rs.wasNull()) {
            auction.setWinnerUserId(winnerUserId);
        }
        Double finalPrice = rs.getDouble("final_price");
        if (!rs.wasNull()) {
            auction.setFinalPrice(finalPrice);
        }
        
        if (includeOwner) {
            auction.setOwnerUsername(rs.getString("owner_username"));
            auction.setWinnerUsername(rs.getString("winner_username"));
        }
        return auction;
    }

    // Update auction status
    public void updateAuctionStatus(int auctionId, String status) throws SQLException {
        String sql = "UPDATE auctions SET status = ? WHERE id = ?";
        
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setString(1, status);
            pstmt.setInt(2, auctionId);
            
            int rowsUpdated = pstmt.executeUpdate();
            if (rowsUpdated == 0) {
                throw new SQLException("Auction not found with id: " + auctionId);
            }
        }
    }

    // Update auction WebSocket URL (returns true if auction was found and updated)
    public boolean updateWebsocketUrl(String auctionId, String websocketUrl) throws SQLException {
        String sql = "UPDATE auctions SET websocket_url = ? WHERE id = ?";
        
        try (
            Connection conn = DatabaseConnection.getConnection();
            PreparedStatement pstmt = conn.prepareStatement(sql)
        ) {
            pstmt.setString(1, websocketUrl);
            pstmt.setString(2, auctionId);
            
            int rowsUpdated = pstmt.executeUpdate();
            return rowsUpdated > 0;
        }
    }
}
