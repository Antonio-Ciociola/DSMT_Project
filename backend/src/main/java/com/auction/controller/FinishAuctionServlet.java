package com.auction.controller;

import com.auction.service.AuctionService;
import com.auction.util.ValidationUtils;
import com.auction.util.ValidationUtils.ValidationException;
import com.auction.model.Auction;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Optional;

/**
 * API endpoint to finish an auction.
 * Accessible without authentication - intended for Erlang service to call.
 * Updates auction status to 'finished', sets winner, and transfers payment.
 */
@WebServlet("/api/finish-auction")
public class FinishAuctionServlet extends HttpServlet {

    // Service instance to interact with auction data
    private final AuctionService auctionService = new AuctionService();

    /**
     * POST /api/finish-auction
     * Parameters:
     *   - auctionId (required): ID of the auction to finish
     *   - winnerId (required): ID of the user who won the auction
     *   - finalPrice (required): Amount paid by the winner
     * 
     * Returns JSON response with success or error message.
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) 
            throws IOException {
        
        // Set response content type to JSON
        response.setContentType("application/json;charset=UTF-8");
        
        try {
            // Extract parameters
            String auctionIdStr = request.getParameter("auctionId");
            String winnerIdStr = request.getParameter("winnerId");
            String finalPriceStr = request.getParameter("finalPrice");
            
            // Validate required parameters and parse/validate values
            String error = ValidationUtils.validateRequired(auctionIdStr, "auctionId");
            if (error != null) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                try (PrintWriter out = response.getWriter()) {
                    out.print("{\"error\": \"" + error + "\"}");
                }
                return;
            }
            
            error = ValidationUtils.validateRequired(winnerIdStr, "winnerId");
            if (error != null) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                try (PrintWriter out = response.getWriter()) {
                    out.print("{\"error\": \"" + error + "\"}");
                }
                return;
            }
            
            error = ValidationUtils.validateRequired(finalPriceStr, "finalPrice");
            if (error != null) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                try (PrintWriter out = response.getWriter()) {
                    out.print("{\"error\": \"" + error + "\"}");
                }
                return;
            }
            
            // Parse and validate numeric values using ValidationUtils
            int auctionId;
            int winnerId;
            double finalPrice;
            
            try {
                auctionId = ValidationUtils.validatePositiveInteger(auctionIdStr, "auctionId");
                winnerId = ValidationUtils.validatePositiveInteger(winnerIdStr, "winnerId");
                finalPrice = ValidationUtils.validatePositiveDouble(finalPriceStr, "finalPrice");
            } catch (ValidationException e) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                try (PrintWriter out = response.getWriter()) {
                    out.print("{\"error\": \"" + e.getMessage() + "\"}");
                }
                return;
            }
            
            // Check if auction exists otherwise return error
            Optional<Auction> auctionOpt = auctionService.getAuctionById(auctionId);
            if (!auctionOpt.isPresent()) {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                try (PrintWriter out = response.getWriter()) {
                    out.print("{\"error\": \"Auction not found for id " + auctionId + "\"}");
                }
                return;
            }
            
            // Check if auction is ongoing otherwise return error
            Auction auction = auctionOpt.get();
            if (!"ongoing".equals(auction.getStatus())) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                try (PrintWriter out = response.getWriter()) {
                    out.printf("{\"error\": \"Auction is not ongoing. Current status: %s\"}%n", auction.getStatus());
                }
                return;
            }
            
            // Finish the auction
            auctionService.finishAuction(auctionId, winnerId, finalPrice);
            
            // Return success response
            response.setStatus(HttpServletResponse.SC_OK);
            try (PrintWriter out = response.getWriter()) {
                out.printf("{\"message\": \"Auction %d finished successfully. Winner: %d, Final Price: $%.2f\"}%n", 
                           auctionId, winnerId, finalPrice);
            }
            
        } 
        // Handle general exceptions
        catch (Exception e) {
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            try (PrintWriter out = response.getWriter()) {
                out.printf("{\"error\": \"Database error: %s\"}%n", e.getMessage());
            }
        }
    }
}
