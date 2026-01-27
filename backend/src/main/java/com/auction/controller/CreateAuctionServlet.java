package com.auction.controller;

import com.auction.model.Auction;
import com.auction.service.AuctionService;
import com.auction.util.ValidationUtils;
import com.auction.util.ValidationUtils.ValidationException;
import com.auction.util.QuartzScheduler;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.quartz.SchedulerException;
import java.io.IOException;
import java.time.LocalDateTime;

/**
 * Servlet implementation class CreateAuctionServlet
 * It handles the creation of new auctions by logged-in users.
 */
@WebServlet("/create-auction")
public class CreateAuctionServlet extends HttpServlet {

    // Service instance to interact with auction data
    private final AuctionService auctionService = new AuctionService();

    /**
     * Handles GET requests to show the create auction page.
     * Redirects guests to login page.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Extract username from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        
        // Redirect user to login if user is not logged in
        if (username == null || "Guest".equals(username)) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }
        
        // Forward to create auction form if user is logged in
        request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
    }

    /**
     * Handles POST requests to process auction creation submissions.
     * Validates input and inserts new auction into the database.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Extract user info and username from session
        HttpSession session = request.getSession(false);
        String username = (session != null) ? (String) session.getAttribute("username") : null;
        Integer userId = (session != null) ? (Integer) session.getAttribute("userId") : null;
        
        // Redirect user to login if user is not logged in
        if (username == null || "Guest".equals(username) || userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }
        
        // Get form parameters
        String title = request.getParameter("title");
        String description = request.getParameter("description");
        String startingPriceStr = request.getParameter("startingPrice");
        String minBidIncrementStr = request.getParameter("minBidIncrement");
        String initialWaitHoursStr = request.getParameter("initialWaitHours");
        String initialWaitMinutesStr = request.getParameter("initialWaitMinutes");
        String initialWaitSecondsStr = request.getParameter("initialWaitSeconds");
        String bidTimeHoursStr = request.getParameter("bidTimeHours");
        String bidTimeMinutesStr = request.getParameter("bidTimeMinutes");
        String bidTimeSecondsStr = request.getParameter("bidTimeSeconds");
        String startDateStr = request.getParameter("startDate");
        String startTimeStr = request.getParameter("startTime");

        // Validate required fields, forward with error if any are missing
        String error = ValidationUtils.validateRequired(title, "Title");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        error = ValidationUtils.validateRequired(startingPriceStr, "Starting price");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        error = ValidationUtils.validateRequired(minBidIncrementStr, "Minimum bid increment");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        error = ValidationUtils.validateRequired(startDateStr, "Start date");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        error = ValidationUtils.validateRequired(startTimeStr, "Start time");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        // Parse and validate numeric/date inputs
        double startingPrice;
        double minBidIncrement;
        int initialWaitTimeSeconds;
        int bidTimeIncrementSeconds;
        LocalDateTime auctionStartDateTime;

        try {
            startingPrice = ValidationUtils.validatePositiveDouble(startingPriceStr, "Starting price");
            minBidIncrement = ValidationUtils.validateStrictlyPositiveDouble(minBidIncrementStr, "Minimum bid increment");
            
            // Parse time components with defaults of 0
            int initialWaitHours = initialWaitHoursStr != null && !initialWaitHoursStr.isEmpty() 
                ? Integer.parseInt(initialWaitHoursStr) : 0;
            int initialWaitMinutes = initialWaitMinutesStr != null && !initialWaitMinutesStr.isEmpty() 
                ? Integer.parseInt(initialWaitMinutesStr) : 0;
            int initialWaitSeconds = initialWaitSecondsStr != null && !initialWaitSecondsStr.isEmpty() 
                ? Integer.parseInt(initialWaitSecondsStr) : 0;
            
            int bidTimeHours = bidTimeHoursStr != null && !bidTimeHoursStr.isEmpty() 
                ? Integer.parseInt(bidTimeHoursStr) : 0;
            int bidTimeMinutes = bidTimeMinutesStr != null && !bidTimeMinutesStr.isEmpty() 
                ? Integer.parseInt(bidTimeMinutesStr) : 0;
            int bidTimeSeconds = bidTimeSecondsStr != null && !bidTimeSecondsStr.isEmpty() 
                ? Integer.parseInt(bidTimeSecondsStr) : 0;
            
            // Convert to total seconds
            initialWaitTimeSeconds = (initialWaitHours * 3600) + (initialWaitMinutes * 60) + initialWaitSeconds;
            bidTimeIncrementSeconds = (bidTimeHours * 3600) + (bidTimeMinutes * 60) + bidTimeSeconds;
            
            // Validate that at least one time value is greater than 0
            if (initialWaitTimeSeconds <= 0) {
                throw new ValidationException("Initial wait time must be greater than 0");
            }
            if (bidTimeIncrementSeconds <= 0) {
                throw new ValidationException("Bid time increment must be greater than 0");
            }
            
            auctionStartDateTime = ValidationUtils.validateFutureDateTime(startDateStr, startTimeStr);
        } catch (ValidationException e) {
            request.setAttribute("errorMessage", e.getMessage());
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        // Create auction object and persist to database
        try {

            // Create new auction object
            Auction auction = new Auction();
            auction.setUserId(userId);
            auction.setTitle(title);
            auction.setDescription(description != null ? description : "");
            auction.setStartingPrice(startingPrice);
            auction.setMinBidIncrement(minBidIncrement);
            auction.setStartDate(auctionStartDateTime);
            auction.setStatus("not_started");
            auction.setInitialWaitTime(initialWaitTimeSeconds);
            auction.setBidTimeIncrement(bidTimeIncrementSeconds);

            // Insert auction into database
            int auctionId = auctionService.createAuction(auction);

            // Schedule the auction to start at the specified date/time
            try {
                QuartzScheduler.scheduleAuctionStart(auctionId, auctionStartDateTime);
            } catch (SchedulerException e) {
                System.err.println("Warning: Failed to schedule auction start: " + e.getMessage());
                // Don't fail the auction creation, just log the warning
            }

            // Redirect to home page on successful creation
            response.sendRedirect(request.getContextPath() + "/home");
        } 
        // Handle general exceptions during auction creation
        catch (Exception e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
        }
    }
}
