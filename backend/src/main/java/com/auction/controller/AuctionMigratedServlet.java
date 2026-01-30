package com.auction.controller;

import com.auction.dao.AuctionDao;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * Servlet to handle auction migration notifications from Erlang master.
 * Updates the WebSocket URL when an auction is migrated to a different slave node.
 */
@WebServlet("/api/auction-migrated")
public class AuctionMigratedServlet extends HttpServlet {

    private final AuctionDao auctionDao = new AuctionDao();
    
    /**
     * Handle POST request to update auction WebSocket URL after migration.
     * Expected JSON body:
     * - auctionId: The ID of the migrated auction
     * - websocketUrl: The new WebSocket URL on the new slave
     * 
     * Requires X-API-Key header for authentication.
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        // Set response content type
        response.setContentType("application/json");
        
        // Validate API key
        String apiKey = request.getHeader("X-API-Key");
        String expectedApiKey = System.getenv().getOrDefault("ERLANG_API_KEY", "auction_secret_key_2026");
        
        if (apiKey == null || !apiKey.equals(expectedApiKey)) {
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write("{\"error\":\"Invalid API key\"}");
            return;
        }
        
        // Read JSON body
        StringBuilder jsonBody = new StringBuilder();
        try (BufferedReader reader = request.getReader()) {
            String line;
            while ((line = reader.readLine()) != null) {
                jsonBody.append(line);
            }
        }
        
        String body = jsonBody.toString();
        System.out.println("[MIGRATION] Received request body: " + body);
        
        // Simple JSON parsing (extract auctionId and websocketUrl)
        String auctionId = extractJsonValue(body, "auctionId");
        String websocketUrl = extractJsonValue(body, "websocketUrl");
        
        // Validate parameters
        if (auctionId == null || auctionId.trim().isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write("{\"error\":\"Missing auctionId parameter\"}");
            return;
        }
        
        if (websocketUrl == null || websocketUrl.trim().isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write("{\"error\":\"Missing websocketUrl parameter\"}");
            return;
        }
        
        try {
            // Update the WebSocket URL in the database
            boolean updated = auctionDao.updateWebsocketUrl(auctionId, websocketUrl);
            
            if (updated) {
                response.setStatus(HttpServletResponse.SC_OK);
                response.getWriter().write("{\"status\":\"success\",\"message\":\"WebSocket URL updated\"}");
                System.out.println("[MIGRATION] Updated WebSocket URL for auction " + auctionId + 
                                 " to " + websocketUrl);
            } else {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.getWriter().write("{\"error\":\"Auction not found\"}");
                System.err.println("[MIGRATION] Auction not found: " + auctionId);
            }
        } catch (Exception e) {
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write("{\"error\":\"Database error\"}");
            System.err.println("[MIGRATION] Error updating WebSocket URL: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Simple JSON value extraction (no library needed for simple cases)
     */
    private String extractJsonValue(String json, String key) {
        String searchKey = "\"" + key + "\":\"";
        int startIndex = json.indexOf(searchKey);
        if (startIndex == -1) {
            return null;
        }
        startIndex += searchKey.length();
        int endIndex = json.indexOf("\"", startIndex);
        if (endIndex == -1) {
            return null;
        }
        return json.substring(startIndex, endIndex);
    }
}
