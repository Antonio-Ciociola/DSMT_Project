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
import java.util.List;

/**
 * Servlet implementation class BrowseAuctionsServlet
 * It handles requests to browse all auctions (ongoing, upcoming, finished).
 */
@WebServlet("/browse-auctions")
public class BrowseAuctionsServlet extends HttpServlet {

    // Service instance to interact with auction data
    private final AuctionService auctionService = new AuctionService();

    /**
     * Handles GET requests to display all auctions.
     * @param request  the HttpServletRequest object
     * @param response the HttpServletResponse object
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException      if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // Fetch all auctions from the service layer
        try {
            List<Auction> auctions = auctionService.getAllAuctions();
            request.setAttribute("auctions", auctions);
        } 
        // Handle SQL exceptions during auction retrieval
        catch (SQLException e) {
            e.printStackTrace();
            request.setAttribute("errorMessage", "Database error. Could not fetch auctions.");
        }

        // Forward to the JSP page for rendering
        request.getRequestDispatcher("/browse-auctions.jsp").forward(request, response);
    }
}
