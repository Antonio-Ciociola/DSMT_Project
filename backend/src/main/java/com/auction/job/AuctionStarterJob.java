package com.auction.job;

import com.auction.dao.AuctionDao;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

import java.sql.SQLException;

// Job class to start an auction by updating its status
public class AuctionStarterJob implements Job {
    
    /**
     * Execute the job to start the auction.
     * @param context The job execution context
     * @throws JobExecutionException If there is an error during job execution
     */
    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        
        try {
            // Get the auction ID from the job data map
            int auctionId = (int) context.getJobDetail().getJobDataMap().get("auctionId");
            
            // Update the auction status to "ongoing"
            AuctionDao auctionDao = new AuctionDao();
            auctionDao.updateAuctionStatus(auctionId, "ongoing");
            
            System.out.println("Auction " + auctionId + " started successfully at " + System.currentTimeMillis());
        } 
        // Handle SQL exceptions during auction start
        catch (SQLException e) {
            System.err.println("Error starting auction: " + e.getMessage());
            throw new JobExecutionException("Failed to start auction", e);
        }
    }
}
