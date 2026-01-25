package com.auction.service;

import com.auction.dao.AuctionDao;
import com.auction.model.Auction;
import java.sql.SQLException;
import java.util.List;

// Service class for managing auction-related operations
public class AuctionService {

    // DAO instance to interact with auction data
    private final AuctionDao auctionDao;

    // Constructor initializing the AuctionDao
    public AuctionService() {
        this.auctionDao = new AuctionDao();
    }

    // Fetch all upcoming auctions with owner details
    public List<Auction> getUpcomingAuctions() throws SQLException {
        return auctionDao.findUpcomingWithOwner();
    }

    // Fetch upcoming auctions for a specific user
    public List<Auction> getUserUpcomingAuctions(int userId) throws SQLException {
        return auctionDao.findUpcomingByUser(userId);
    }

    // Create a new auction
    public void createAuction(Auction auction) throws SQLException {
        auctionDao.createAuction(auction);
    }

    // Delete an upcoming auction owned by a specific user
    public boolean deleteUserUpcomingAuction(int auctionId, int userId) throws SQLException {
        return auctionDao.deleteUpcomingOwnedAuction(auctionId, userId);
    }
}
