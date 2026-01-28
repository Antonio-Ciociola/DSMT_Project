package com.auction.controller;

import com.auction.model.Auction;
import com.auction.service.AuctionService;
import com.auction.service.UserService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.math.BigDecimal;
import java.sql.SQLException;
import java.util.Optional;

/**
 * Servlet for joining an auction
 */
@WebServlet("/join-auction")
public class JoinAuctionServlet extends HttpServlet {
    
    private final UserService userService = new UserService();
    private final AuctionService auctionService = new AuctionService();
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Set JSON content type for all responses
        response.setContentType("application/json");
        
        // Get session and user info
        HttpSession session = request.getSession(false);
        boolean isGuest = (session == null);
        
        Integer userId = null;
        String username = null;
        
        if (!isGuest) {
            userId = (Integer) session.getAttribute("userId");
            username = (String) session.getAttribute("username");
            isGuest = (userId == null || username == null);
        }
        
        // Get auction ID from request
        String auctionIdStr = request.getParameter("auctionId");
        if (auctionIdStr == null || auctionIdStr.trim().isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write("{\"error\": \"Auction ID is required\"}");
            return;
        }
        
        System.out.println("Join Auction called!");
        System.out.println("User ID: " + userId);
        System.out.println("Username: " + username);
        System.out.println("Auction ID: " + auctionIdStr);
        
        try {
            // Parse auction ID
            int auctionId;
            try {
                auctionId = Integer.parseInt(auctionIdStr);
            } catch (NumberFormatException e) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.getWriter().write("{\"error\": \"Invalid auction ID format\"}");
                return;
            }
            
            // Check if auction exists and is ongoing
            Optional<Auction> auctionOpt = auctionService.getAuctionById(auctionId);
            if (!auctionOpt.isPresent()) {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.getWriter().write("{\"error\": \"Auction not found\"}");
                return;
            }
            
            Auction auction = auctionOpt.get();
            String auctionStatus = auction.getStatus();
            
            // Check if user owns this auction - if so, force guest mode
            if (!isGuest && userId != null && auction.getUserId() == userId) {
                System.out.println("User " + userId + " owns auction " + auctionId + " - joining as guest (view-only)");
                isGuest = true;
                username = "guest_owner_" + userId;
            }
            
            // Only allow joining if auction is ongoing
            if (!"ongoing".equalsIgnoreCase(auctionStatus)) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.getWriter().write(
                    "{\"error\": \"Cannot join auction\", " +
                    "\"reason\": \"Auction is not ongoing\", " +
                    "\"status\": \"" + auctionStatus + "\"}"
                );
                return;
            }
            
            System.out.println("Auction status: " + auctionStatus + " - OK to join");
            
            // Get user balance from MySQL (only if not guest)
            BigDecimal balance = BigDecimal.ZERO;
            if (!isGuest) {
                balance = userService.getBalance(userId);
                System.out.println("User balance: " + balance);
                
                // Handle null balance
                if (balance == null) {
                    System.err.println("Balance is null for user " + userId + ", defaulting to 0");
                    balance = BigDecimal.ZERO;
                }
            } else {
                System.out.println("Guest user - balance set to 0");
            }
            
            // Generate JWT token with auction-specific info
            String jwtToken = com.auction.util.JwtUtil.generateToken(
                username != null ? username : "guest", 
                auctionIdStr,
                balance.doubleValue(),
                isGuest
            );
            System.out.println("Generated JWT for auction join: " + jwtToken.substring(0, 20) + "...");
            
            // Store JWT in session
            session.setAttribute("jwtToken", jwtToken);
            session.setAttribute("currentAuctionId", auctionIdStr);
            
            // Get WebSocket URL from database (saved when auction started)
            String websocketUrl = auction.getWebsocketUrl();
            
            if (websocketUrl == null || websocketUrl.trim().isEmpty()) {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.getWriter().write("{\"error\": \"Auction WebSocket not available. Please wait for auction to start.\"}");
                return;
            }
            
            System.out.println("WebSocket URL: " + websocketUrl);
            
            // Return success with JWT and WebSocket URL
            response.getWriter().write(
                "{\"success\": true, \"message\": \"Successfully joined auction\", " +
                "\"auctionId\": \"" + auctionIdStr + "\", " +
                "\"userId\": \"" + userId + "\", " +
                "\"balance\": " + balance.toString() + ", " +
                "\"jwtToken\": \"" + jwtToken + "\", " +
                "\"websocketUrl\": \"" + websocketUrl + "\"}"
            );
            
        } catch (SQLException e) {
            System.err.println("Database error: " + e.getMessage());
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write("{\"error\": \"Database error: " + e.getMessage() + "\"}");
        } catch (Exception e) {
            System.err.println("Error generating JWT: " + e.getMessage());
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write("{\"error\": \"Error joining auction: " + e.getMessage() + "\"}");
        }
    }
}
