package com.auction.controller;

import com.auction.model.Auction;
import com.auction.service.AuctionService;
import com.auction.util.ValidationUtils;
import com.auction.util.ValidationUtils.ValidationException;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
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
        String initialWaitTimeStr = request.getParameter("initialWaitTime");
        String bidTimeIncrementStr = request.getParameter("bidTimeIncrement");
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

        error = ValidationUtils.validateRequired(initialWaitTimeStr, "Initial wait time");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        error = ValidationUtils.validateRequired(bidTimeIncrementStr, "Bid time increment");
        if (error != null) {
            request.setAttribute("errorMessage", error);
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }

        // Parse and validate numeric/date inputs
        double startingPrice;
        double minBidIncrement;
        int initialWaitTime;
        int bidTimeIncrement;
        LocalDateTime auctionStartDateTime;

        try {
            startingPrice = ValidationUtils.validatePositiveDouble(startingPriceStr, "Starting price");
            minBidIncrement = ValidationUtils.validateStrictlyPositiveDouble(minBidIncrementStr, "Minimum bid increment");
            initialWaitTime = ValidationUtils.validatePositiveInteger(initialWaitTimeStr, "Initial wait time");
            bidTimeIncrement = ValidationUtils.validatePositiveInteger(bidTimeIncrementStr, "Bid time increment");
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
            auction.setInitialWaitTime(initialWaitTime);
            auction.setBidTimeIncrement(bidTimeIncrement);

            // Insert auction into database
            auctionService.createAuction(auction);

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
