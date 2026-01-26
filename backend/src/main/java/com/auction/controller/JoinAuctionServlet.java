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
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
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
        
        // Get session and user info
        HttpSession session = request.getSession(false);
        if (session == null) {
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.setContentType("application/json");
            response.getWriter().write("{\"error\": \"Not logged in\"}");
            return;
        }
        
        Integer userId = (Integer) session.getAttribute("userId");
        String username = (String) session.getAttribute("username");
        
        if (userId == null || username == null) {
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.setContentType("application/json");
            response.getWriter().write("{\"error\": \"Not logged in\"}");
            return;
        }
        
        // Get auction ID from request
        String auctionIdStr = request.getParameter("auctionId");
        if (auctionIdStr == null || auctionIdStr.trim().isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.setContentType("application/json");
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
                response.setContentType("application/json");
                response.getWriter().write("{\"error\": \"Invalid auction ID format\"}");
                return;
            }
            
            // Check if auction exists and is ongoing
            Optional<Auction> auctionOpt = auctionService.getAuctionById(auctionId);
            if (!auctionOpt.isPresent()) {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.setContentType("application/json");
                response.getWriter().write("{\"error\": \"Auction not found\"}");
                return;
            }
            
            Auction auction = auctionOpt.get();
            String auctionStatus = auction.getStatus();
            
            // Only allow joining if auction is ongoing
            if (!"ongoing".equalsIgnoreCase(auctionStatus)) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.setContentType("application/json");
                response.getWriter().write(
                    "{\"error\": \"Cannot join auction\", " +
                    "\"reason\": \"Auction is not ongoing\", " +
                    "\"status\": \"" + auctionStatus + "\"}"
                );
                return;
            }
            
            System.out.println("Auction status: " + auctionStatus + " - OK to join");
            
            // Get user balance from MySQL
            BigDecimal balance = userService.getBalance(userId);
            System.out.println("User balance: " + balance);
            
            // Prepare JSON payload for Erlang endpoint
            String jsonPayload = String.format(
                "{\"userid\": \"%s\", \"auction_id\": \"%s\", \"balance\": %s}",
                userId, auctionIdStr, balance.toString()
            );
            
            System.out.println("Sending to Erlang: " + jsonPayload);
            
            // POST to Erlang endpoint
            String erlangUrl = "http://erlang:8081/erlangapi/user";
            URL url = new URL(erlangUrl);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type", "application/json");
            conn.setDoOutput(true);
            
            // Send request
            try (OutputStream os = conn.getOutputStream()) {
                byte[] input = jsonPayload.getBytes(StandardCharsets.UTF_8);
                os.write(input, 0, input.length);
            }
            
            // Get response from Erlang
            int erlangResponseCode = conn.getResponseCode();
            System.out.println("Erlang response code: " + erlangResponseCode);
            
            if (erlangResponseCode == HttpURLConnection.HTTP_OK || 
                erlangResponseCode == HttpURLConnection.HTTP_CREATED) {
                // Success
                response.setContentType("application/json");
                response.getWriter().write(
                    "{\"success\": true, \"message\": \"Successfully joined auction\", " +
                    "\"auctionId\": \"" + auctionIdStr + "\", " +
                    "\"userId\": \"" + userId + "\", " +
                    "\"balance\": " + balance.toString() + "}"
                );
            } else {
                // Erlang returned error
                response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                response.setContentType("application/json");
                response.getWriter().write(
                    "{\"error\": \"Failed to register with auction system\", " +
                    "\"erlangStatus\": " + erlangResponseCode + "}"
                );
            }
            
        } catch (SQLException e) {
            System.err.println("Database error: " + e.getMessage());
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.setContentType("application/json");
            response.getWriter().write("{\"error\": \"Database error: " + e.getMessage() + "\"}");
        } catch (Exception e) {
            System.err.println("Error calling Erlang: " + e.getMessage());
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.setContentType("application/json");
            response.getWriter().write("{\"error\": \"Error connecting to auction system: " + e.getMessage() + "\"}");
        }
    }
}
