package com.auction.controller;

import com.auction.model.DatabaseConnection;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Servlet implementation class CreateAuctionServlet
 * It handles the creation of new auctions by logged-in users.
 */
@WebServlet("/create-auction")
public class CreateAuctionServlet extends HttpServlet {

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
        String countdownTimerStr = request.getParameter("countdownTimer");
        String startDateStr = request.getParameter("startDate");
        String startTimeStr = request.getParameter("startTime");
    
        // If any field is missing, return to create auction with error message
        if (title == null || title.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Title is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        if (startingPriceStr == null || startingPriceStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Starting price is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        if (minBidIncrementStr == null || minBidIncrementStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Minimum bid increment is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        if (countdownTimerStr == null || countdownTimerStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Countdown timer is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        if (startDateStr == null || startDateStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Start date is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        if (startTimeStr == null || startTimeStr.trim().isEmpty()) {
            request.setAttribute("errorMessage", "Start time is required.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        // Parse and validate inputs
        double startingPrice;
        double minBidIncrement;
        int countdownTimer;
        
        // Parsing starting price, min bid increment, and countdown timer, forwarding with error if invalid
        try {
            startingPrice = Double.parseDouble(startingPriceStr);
            if (startingPrice < 0) {
                throw new NumberFormatException("Price cannot be negative");
            }
        } catch (NumberFormatException e) {
            request.setAttribute("errorMessage", "Invalid starting price.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        try {
            minBidIncrement = Double.parseDouble(minBidIncrementStr);
            if (minBidIncrement <= 0) {
                throw new NumberFormatException("Bid increment must be positive");
            }
        } catch (NumberFormatException e) {
            request.setAttribute("errorMessage", "Invalid minimum bid increment.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        try {
            countdownTimer = Integer.parseInt(countdownTimerStr);
            if (countdownTimer <= 0) {
                throw new NumberFormatException("Timer must be positive");
            }
        } catch (NumberFormatException e) {
            request.setAttribute("errorMessage", "Invalid countdown timer (must be positive hours).");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        
        // Validate that the start date is not in the past, forwarding with error if invalid
        String startDatetime = startDateStr + " " + startTimeStr;
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            LocalDateTime auctionStartDateTime = LocalDateTime.parse(startDatetime, formatter);
            LocalDateTime now = LocalDateTime.now();
            
            if (auctionStartDateTime.isBefore(now)) {
                request.setAttribute("errorMessage", "Start date and time cannot be in the past.");
                request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
                return;
            }
        } catch (DateTimeParseException e) {
            request.setAttribute("errorMessage", "Invalid date or time format.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
            return;
        }
        
        
        // Insert auction into database using prepared statement
        try (Connection conn = DatabaseConnection.getConnection()) {

            // Create SQL insert statement
            String sql = "INSERT INTO auctions (user_id, title, description, starting_price, min_bid_increment, countdown_timer, start_date) VALUES (?, ?, ?, ?, ?, ?, ?)";
            PreparedStatement pstmt = conn.prepareStatement(sql);

            // Set parameters
            pstmt.setInt(1, userId);
            pstmt.setString(2, title);
            pstmt.setString(3, description != null ? description : "");
            pstmt.setDouble(4, startingPrice);
            pstmt.setDouble(5, minBidIncrement);
            pstmt.setInt(6, countdownTimer);
            pstmt.setString(7, startDatetime);
            
            // Execute insert
            pstmt.executeUpdate();
            pstmt.close();
            
            // Redirect to home page
            response.sendRedirect(request.getContextPath() + "/home");
            
        } 
        // Handle SQL exceptions, forward back to create auction with error
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Please try again later.");
            request.getRequestDispatcher("/create-auction.jsp").forward(request, response);
        }
    }
}
