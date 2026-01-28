package com.auction.service;

import com.auction.dao.AuctionDao;
import com.auction.dao.UserDao;
import com.auction.model.Auction;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

// Service class for managing auction-related operations
public class AuctionService {

    // DAO instances to interact with data
    private final AuctionDao auctionDao;
    private final UserDao userDao;

    // Constructor initializing the DAOs
    public AuctionService() {
        this.auctionDao = new AuctionDao();
        this.userDao = new UserDao();
    }

    // Fetch all auctions with owner details
    public List<Auction> getAllAuctions() throws SQLException {
        return auctionDao.findAllWithOwner();
    }

    // Fetch upcoming auctions for a specific user
    public List<Auction> getUserUpcomingAuctions(int userId) throws SQLException {
        return auctionDao.findUpcomingByUser(userId);
    }

    // Fetch auction by ID
    public Optional<Auction> getAuctionById(int auctionId) throws SQLException {
        return auctionDao.findById(auctionId);
    }

    // Create a new auction
    public int createAuction(Auction auction) throws SQLException {
        return auctionDao.createAuction(auction);
    }

    // Delete an upcoming auction owned by a specific user
    public boolean deleteUserUpcomingAuction(int auctionId, int userId) throws SQLException {
        return auctionDao.deleteUpcomingOwnedAuction(auctionId, userId);
    }

    // Finish an auction: update status, transfer balance from winner to seller
    public void finishAuction(int auctionId, String winnerUsername, double finalPrice, int totalDuration) throws SQLException {
        
        // Get auction to find the seller
        Optional<Auction> auctionOpt = auctionDao.findById(auctionId);
        if (!auctionOpt.isPresent()) {
            throw new SQLException("Auction not found for id " + auctionId);
        }
        
        Auction auction = auctionOpt.get();
        int sellerId = auction.getUserId();
        
        // Look up winner's user ID from username
        int winnerUserId = userDao.getUserIdByUsername(winnerUsername);
        if (winnerUserId == 0) {
            throw new SQLException("Winner user not found: " + winnerUsername);
        }
        
        // Update auction status and winner info
        auctionDao.finishAuction(auctionId, winnerUserId, finalPrice, totalDuration);
        
        // Transfer balance from winner to seller
        BigDecimal amount = BigDecimal.valueOf(finalPrice);
        userDao.transferBalance(winnerUserId, sellerId, amount);
        
        // Clear auction_id_bidding for all users who were in this auction
        userDao.clearAuctionIdBiddingForAuction(auctionId);
        System.out.format("[SERVICE] Cleared auction_id_bidding for auction %d%n", auctionId);
    }
    
    // Finish an auction with no winner (no bids received)
    public void finishAuctionWithNoWinner(int auctionId, int totalDuration) throws SQLException {
        // Update auction status to finished with no winner
        auctionDao.finishAuctionWithNoWinner(auctionId, totalDuration);
        
        // Clear auction_id_bidding for all users who were in this auction
        userDao.clearAuctionIdBiddingForAuction(auctionId);
        System.out.format("[SERVICE] Cleared auction_id_bidding for auction %d (no winner)%n", auctionId);
    }
}
