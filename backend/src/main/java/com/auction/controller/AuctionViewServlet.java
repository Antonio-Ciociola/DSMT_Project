package com.auction.controller;

import com.auction.model.Auction;
import com.auction.service.AuctionService;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Optional;

/**
 * Servlet for viewing a specific auction
 */
@WebServlet("/auction/*")
public class AuctionViewServlet extends HttpServlet {
    
    private final AuctionService auctionService = new AuctionService();
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) 
            throws ServletException, IOException {
        
        // Extract auction ID from path: /auction/123
        String pathInfo = request.getPathInfo();
        if (pathInfo == null || pathInfo.equals("/")) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Auction ID is required");
            return;
        }
        
        // Remove leading slash and parse ID
        String auctionIdStr = pathInfo.substring(1);
        int auctionId;
        
        try {
            auctionId = Integer.parseInt(auctionIdStr);
        } catch (NumberFormatException e) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Invalid auction ID format");
            return;
        }
        
        try {
            // Get auction from database
            Optional<Auction> auctionOpt = auctionService.getAuctionById(auctionId);
            
            if (!auctionOpt.isPresent()) {
                response.sendError(HttpServletResponse.SC_NOT_FOUND, "Auction not found");
                return;
            }
            
            Auction auction = auctionOpt.get();
            String status = auction.getStatus();
            
            // Only show ongoing auctions
            if (!"ongoing".equalsIgnoreCase(status)) {
                request.setAttribute("errorMessage", 
                    "This auction is not available. Status: " + status);
                request.setAttribute("auctionStatus", status);
                request.getRequestDispatcher("/auction-unavailable.jsp").forward(request, response);
                return;
            }
            
            // Auction is ongoing - show the auction page
            request.setAttribute("auction", auction);
            request.getRequestDispatcher("/auction-view.jsp").forward(request, response);
            
        } catch (SQLException e) {
            System.err.println("Database error in AuctionViewServlet: " + e.getMessage());
            e.printStackTrace();
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, 
                "Error retrieving auction information");
        }
    }
}
