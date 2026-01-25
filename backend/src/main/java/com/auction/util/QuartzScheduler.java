package com.auction.util;

import com.auction.job.AuctionStarterJob;
import org.quartz.*;
import org.quartz.impl.StdSchedulerFactory;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

public class QuartzScheduler {

    private static Scheduler scheduler;
    
    // Initialize the Quartz scheduler
    static {
        try {
            scheduler = new StdSchedulerFactory().getScheduler();
            if (!scheduler.isStarted()) {
                scheduler.start();
            }
        } catch (SchedulerException e) {
            System.err.println("Failed to initialize Quartz scheduler: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Schedule an auction to start at the specified date/time.
     * 
     * @param auctionId The ID of the auction to schedule
     * @param startDateTime The LocalDateTime when the auction should start
     * @throws SchedulerException If scheduling fails
     */
    public static void scheduleAuctionStart(int auctionId, LocalDateTime startDateTime) throws SchedulerException {
        
        // Ensure scheduler is initialized
        if (scheduler == null) {
            throw new SchedulerException("Scheduler not initialized");
        }
        
        // Convert LocalDateTime to Date (using system default timezone)
        Date triggerTime = Date.from(startDateTime.atZone(ZoneId.systemDefault()).toInstant());
        
        // Create a unique job name for this auction
        String jobName = "auction-starter-" + auctionId;
        String triggerName = "trigger-" + auctionId;
        
        // Create the job detail
        JobDetail job = JobBuilder.newJob(AuctionStarterJob.class)
                .withIdentity(jobName)
                .usingJobData("auctionId", auctionId)
                .build();
        
        // Create the trigger (one-time trigger at the specified time)
        Trigger trigger = TriggerBuilder.newTrigger()
                .withIdentity(triggerName)
                .startAt(triggerTime)
                .build();
        
        // Schedule the job
        scheduler.scheduleJob(job, trigger);
        System.out.println("Scheduled auction " + auctionId + " to start at " + startDateTime);
    }
    
    /**
     * Cancel a scheduled auction job.
     * 
     * @param auctionId The ID of the auction to cancel
     * @throws SchedulerException If cancellation fails
     */
    public static void cancelAuctionStart(int auctionId) throws SchedulerException {
        
        // Ensure scheduler is initialized
        if (scheduler == null) {
            throw new SchedulerException("Scheduler not initialized");
        }
        
        // Delete the job using its unique name
        String jobName = "auction-starter-" + auctionId;
        scheduler.deleteJob(new JobKey(jobName));
        System.out.println("Cancelled scheduled auction " + auctionId);
    }
}
